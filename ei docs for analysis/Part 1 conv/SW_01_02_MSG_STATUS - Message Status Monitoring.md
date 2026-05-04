# Exception Indicator: Message Status Monitoring (SW_01_02_MSG_STATUS)


## General Overview

This Exception Indicator (EI) provides comprehensive monitoring and analysis of SAP message status records to ensure reliable output processing, document generation, and communication delivery through the SAP Message Control framework. Message status monitoring is essential for maintaining business process continuity and ensuring that critical business documents, reports, and communications are processed and delivered successfully to intended recipients.

Message Status Monitoring extends the standard SAP Output Management framework by providing detailed message processing lifecycle tracking and delivery status analysis. The technology enables proactive monitoring of message processing events including failed outputs, stuck processing, delivery confirmations, and communication channel performance, crucial for maintaining robust business communication workflows and preventing document delivery failures.

This Exception Indicator provides detailed message status monitoring capabilities that enable:

Output processing tracking to monitor the status and performance of business document generation and delivery

Message delivery analysis for understanding communication success patterns and identifying processing failures

Processing bottleneck detection to identify stuck or delayed message processing that may impact business operations

Communication channel monitoring to track delivery performance across different output types and channels

Business process continuity for ensuring critical business communications reach their intended destinations

The monitoring solution analyzes message status data from the NAST table (Message Status), similar to data available through VN01/VN02 transactions (Output Control), and provides enhanced filtering capabilities to focus on specific applications, output types, processing statuses, and time periods. This enables targeted analysis of message processing characteristics and identification of output-related business continuity issues.

This Exception Indicator checks whether SAP message processing is functioning reliably and identifies potential issues that may impact business document delivery, communication workflows, and operational continuity.


## Problem Description

Poor message processing performance and delivery failures indicate output management system issues causing:

Communication and Document Delivery Problems

Failed message processing preventing critical business document delivery and communication workflows

Stuck output processing causing document generation delays and recipient notification failures

Message delivery errors leading to incomplete business process execution and communication gaps

Output channel failures disrupting automated business document distribution and operational workflows

Business Process Issues

Document processing bottlenecks affecting business operation timelines and customer communication

Message delivery failures impacting compliance requirements and business process completion

Output generation delays causing customer service issues and operational inefficiencies

Communication workflow interruptions affecting business relationship management and process automation

Business Impact

Business continuity disruptions due to failed or delayed message delivery affecting operational effectiveness

Compliance violations from missing or undelivered business documents impacting regulatory adherence

Customer satisfaction issues from communication delivery failures affecting business relationships and service quality

Process automation failures from unreliable message processing compromising operational efficiency and productivity


## Suggested Resolution

Immediate Response

Investigate failed and stuck message processing using VN01/VN02 transactions for detailed output analysis and resolution

Check message processing queues and delivery channels for bottleneck identification and immediate remediation

Review output management system performance and resource utilization for capacity assessment

Analyze message delivery patterns and identify frequently failing output types for targeted troubleshooting

System Assessment

Monitor message processing performance trends and delivery success rates for optimization opportunities

Evaluate output management system capacity and resource allocation for workload optimization

Check communication channel configuration and delivery mechanism reliability for performance enhancement

Analyze message processing patterns by application, output type, and delivery channel for system tuning

Corrective Actions

Optimize message processing parameters and output management configurations for improved reliability and performance

Implement message delivery error handling and retry mechanisms for enhanced fault tolerance and recovery

Establish proactive message status monitoring and alerting procedures for early issue detection and prevention

Plan system capacity upgrades and process improvements based on message volume analysis and delivery requirements


## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | ACALL | Not used | CHAR | 1 | 0 | NA_OBS001 | CHAR1 |
| 2 | ADRNR | Address Number | CHAR | 10 | 0 | AD_ADDRNUM | AD_ADDRNUM |
| 3 | AENDE | Change message | CHAR | 1 | 0 | NA_AENDE | NA_AENDE |
| 4 | AKTIV | Activity flag | CHAR | 1 | 0 | NA_AKTIV | CHAR1 |
| 5 | ANZAL | Number of messages | DEC | 2 | 0 | NA_ANZAL | DEC2 |
| 6 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 7 | CMFPNR | Number | NUMC | 12 | 0 | NA_CMF_NR | NUMC12 |
| 8 | DATRE | Response date | DATS | 8 | 0 | NA_DATRE | DATUM |
| 9 | DATUM | Date |  | 0 | 0 |  |  |
| 10 | DATVR | Processing date | DATS | 8 | 0 | NA_DATVR | DATUM |
| 11 | DELET | Release after output | CHAR | 1 | 0 | TDDELETE | TDBOOL |
| 12 | DIMME | Print immediately | CHAR | 1 | 0 | TDIMMED | TDBOOL |
| 13 | DSNAM | Spool request name | CHAR | 6 | 0 | RSPO0NAME | CHAR6 |
| 14 | DSUF1 | Suffix 1 | CHAR | 4 | 0 | RSPO1NAME | CHAR4 |
| 15 | DSUF2 | Suffix 2 | CHAR | 12 | 0 | RSPO2NAME | CHAR12 |
| 16 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 17 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 18 | EDIKEY | Not used | CHAR | 8 | 0 | NA_OBS008 | CHAR8 |
| 19 | EDITRANS | Not used | CHAR | 6 | 0 | NA_OBS006 | CHAR6 |
| 20 | ERDAT | Created Date | DATS | 8 | 0 | NA_ERDAT | DATUM |
| 21 | ERUHR | Created Time | TIMS | 6 | 0 | NA_ERUHR | UZEIT |
| 22 | EVENT | Event | CHAR | 32 | 0 | SWO_EVENT | SWC_ELEM |
| 23 | FORFB | Requested Status | CHAR | 1 | 0 | BCS_RQST | BCS_RQST |
| 24 | KAPPL | Application | CHAR | 2 | 0 | SNA_KAPPL | CHAR2 |
| 25 | KSCHL | Mess Type | CHAR | 4 | 0 | SNA_KSCHL | CHAR4 |
| 26 | LDEST | Spool: Output device | CHAR | 4 | 0 | RSPOPNAME | RSPOPNAME |
| 27 | MANUE | Message manually | CHAR | 1 | 0 | NA_MANUE | NA_MANUE |
| 28 | NACHA | Message transmission medium | CHAR | 1 | 0 | NA_NACHA | NA_NACHA |
| 29 | NAUTO | Automatic | CHAR | 1 | 0 | NA_NAUTO | XFELD |
| 30 | OBJAC | Not used | CHAR | 1 | 0 | NA_OBS001 | CHAR1 |
| 31 | OBJCP | Not used | CHAR | 1 | 0 | NA_OBS001 | CHAR1 |
| 32 | OBJKY | Object Key | CHAR | 30 | 0 | NA_OBJKEY | NA_OBJKEY |
| 33 | OBJPRI | Not used | CHAR | 1 | 0 | NA_OBS001 | CHAR1 |
| 34 | OBJRN | Not used | CHAR | 1 | 0 | NA_OBS001 | CHAR1 |
| 35 | OBJRR | Not used | CHAR | 1 | 0 | NA_OBS001 | CHAR1 |
| 36 | OBJSNS | Not used | CHAR | 1 | 0 | NA_OBS001 | CHAR1 |
| 37 | OBJTYPE | Object type | CHAR | 10 | 0 | OJ_NAME | OJ_NAME |
| 38 | OPTARCNR | Archive number | NUMC | 10 | 0 | NA_OPTAR | NA_OPTAR |
| 39 | OUTFL | Not used | CHAR | 1 | 0 | NA_OBS001 | CHAR1 |
| 40 | PARNR | Mess Partner | CHAR | 10 | 0 | NA_PARNR | NA_PARNR |
| 41 | PARVW | Partner Role | CHAR | 2 | 0 | SNA_PARVW | CHAR2 |
| 42 | PFLD1 | Not used | CHAR | 30 | 0 | NA_OBS030 | CHAR30 |
| 43 | PFLD2 | Not used | CHAR | 30 | 0 | NA_OBS030 | CHAR30 |
| 44 | PFLD3 | Not used | CHAR | 30 | 0 | NA_OBS030 | CHAR30 |
| 45 | PFLD4 | SAPscript Form | CHAR | 30 | 0 | NA_TDFORM | CHAR30 |
| 46 | PFLD5 | Layout module for print formatting | CHAR | 30 | 0 | NA_LABEL | FUNCNAME |
| 47 | PMID1 | Not used | CHAR | 3 | 0 | NA_OBS003 | CHAR03 |
| 48 | PMID2 | Not used | CHAR | 3 | 0 | NA_OBS003 | CHAR03 |
| 49 | PMID3 | Not used | CHAR | 3 | 0 | NA_OBS003 | CHAR03 |
| 50 | PMID4 | Not used | CHAR | 3 | 0 | NA_OBS003 | CHAR03 |
| 51 | PMID5 | Not used | CHAR | 3 | 0 | NA_OBS003 | CHAR03 |
| 52 | PRIFB | Report Status by Mail | CHAR | 1 | 0 | BCS_STML | BCS_STML |
| 53 | REPET | Repeatability | CHAR | 1 | 0 | NA_REPET | NA_REPET |
| 54 | REPID | Not used | CHAR | 8 | 0 | NA_OBS008 | CHAR8 |
| 55 | SNDBC | Address Type | CHAR | 1 | 0 | AD_ADRTYPE | AD_ADRTYPE |
| 56 | SNDCP | Not used | CHAR | 1 | 0 | NA_OBS001 | CHAR1 |
| 57 | SNDDR | Send as disregard | CHAR | 1 | 0 | SO_SND_DR | SO_FLAG |
| 58 | SNDEX | Not used | CHAR | 1 | 0 | NA_OBS001 | CHAR1 |
| 59 | SORT1 | Sort criterion | CHAR | 20 | 0 | NAST_SORTK | CHAR20 |
| 60 | SORT2 | Sort criterion | CHAR | 20 | 0 | NAST_SORTK | CHAR20 |
| 61 | SORT3 | Sort criterion | CHAR | 20 | 0 | NAST_SORTK | CHAR20 |
| 62 | SPRAS | Language | LANG | 1 | 0 | NA_SPRAS | SPRAS |
| 63 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 64 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 65 | STATUS_DESC | Short text | CHAR | 60 | 0 | EDI_TEXT60 | TEXT60 |
| 66 | TCODE | Communication strategy | CHAR | 4 | 0 | CSTRATEGY | CSTRATEGY |
| 67 | TDARMOD | Storage Mode | CHAR | 1 | 0 | SYARMOD | SYARMOD |
| 68 | TDAUTORITY | Authorization | CHAR | 12 | 0 | SYPRBER | SYCHAR12 |
| 69 | TDCOVTITLE | Cover Page Text | CHAR | 68 | 0 | SYPRTXT | SYCHAR68K |
| 70 | TDDIVISION | Department on Cover Page | CHAR | 12 | 0 | SYPRABT | SYCHAR12K |
| 71 | TDID | Text ID | CHAR | 4 | 0 | TDID | TDID |
| 72 | TDNAME | Object key | CHAR | 70 | 0 | NA_OBJKY_L | NA_OBJKY_L |
| 73 | TDOCOVER | SAP cover page | CHAR | 1 | 0 | SYPRSAP | SYPRSAP |
| 74 | TDRECEIVER | Recipient | CHAR | 12 | 0 | SYPRREC | SYCHAR12 |
| 75 | TDSCHEDULE | Send time | CHAR | 3 | 0 | SKSCHEDULE | SKSCHEDULE |
| 76 | TDSPRAS | Language Key | LANG | 1 | 0 | SPRAS | SPRAS |
| 77 | TELFX | Fax number | CHAR | 31 | 0 | NA_TELFX | TEXT31 |
| 78 | TELTX | Teletex number | CHAR | 30 | 0 | TELTX | TEXT30 |
| 79 | TELX1 | Telex number | CHAR | 30 | 0 | TELX1 | TEXT30 |
| 80 | TLAND | Country Key | CHAR | 3 | 0 | LAND1 | LAND1 |
| 81 | UHRVR | Processing time | TIMS | 6 | 0 | NA_UHRVR | UZEIT |
| 82 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 83 | USNAM | User name | CHAR | 12 | 0 | USNAM | XUBNAME |
| 84 | USRNAM | Company Address | CHAR | 12 | 0 | NA_PERS_12 | CHAR12 |
| 85 | VSDAT | Send date | DATS | 8 | 0 | NA_VSDAT | DATUM |
| 86 | VSTAT | Processing status of message | CHAR | 1 | 0 | NA_VSTAT | NA_VSTAT |
| 87 | VSURA | Dispatch time from | TIMS | 6 | 0 | NA_VSURA | UZEIT |
| 88 | VSURB | Send time to | TIMS | 6 | 0 | NA_VSURB | UZEIT |
| 89 | VSZTP | Dispatch time | CHAR | 1 | 0 | NA_VSZTP | NA_VSZTP |


### Parameter Configuration Guidelines

IMPORTANT: Configure ALL 89 parameters listed in the Parameters Reference Table when tuning this EI; each influences which records are read, filtered, aged, and surfaced for alerting.

ACALL (Not used)

For distributed landscapes, not used on ACALL often anchors which application server or destination appears in results.

ADRNR (Address Number)

Explains why two monitoring passes differ: only the pass with stricter address number on ADRNR surfaces the disputed rows.

AENDE (Change message)

Reduces false positives during peak windows by tightening change message through AENDE alongside state filters.

AKTIV (Activity flag)

Reflects real administration where activity flag on AKTIV is routinely restricted to a single productive client or object family.

ANZAL (Number of messages)

When populated, keeps the extract focused so number of messages (ANZAL) aligns with the intended triage slice.

BACKDAYS (Back Days)

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 - today, 1 - today + yesterday etc.

Backdays is based on ERDAT

CMFPNR (Number)

Aligns exception volume with the chosen scope by testing number via CMFPNR before alert evaluation.

DATRE (Response date)

Guards against oversized extracts when response date on DATRE is narrowed together with client, user, or session filters.

DATUM (Date)

Guards against oversized extracts when date on DATUM is narrowed together with client, user, or session filters.

DATVR (Processing date)

For operations, processing date on DATVR indicates whether a row belongs in the current monitoring pass versus historical noise.

DELET (Release after output)

Works downstream of the initial read so release after output on DELET still participates in row-level deletion rules.

DIMME (Print immediately)

For distributed landscapes, print immediately on DIMME often anchors which application server or destination appears in results.

DSNAM (Spool request name)

Improves readability of exported lists because spool request name (DSNAM) columns stay aligned with the configured filter intent.

DSUF1 - DSUF2 (Suffix 1 / Suffix 2)

Together, DSUF1 and DSUF2 narrow spool-related naming suffix dimensions so monitoring stays aligned with how operators identify related output requests in the same run.

DURATION (Duration In Time Units)

Helps monitoring stay readable by requiring duration in time units (DURATION) to match organizational or technical selectors when set.

DURATION_UNIT (Duration Unit(D/H/M))

Unit for elapsed time between each session's creation date and time and the evaluation clock.

DURATION_UNIT Options:

·        H — Hours.

·        M — Minutes (preset in code before the selection read when not overridden).

·        D — Days.

·        F — Full-day style counting where applicable to the duration helper.

EDIKEY (Not used)

Reflects real administration where not used on EDIKEY is routinely restricted to a single productive client or object family.

EDITRANS (Not used)

When harmonized with related filters, not used on EDITRANS isolates the highest-risk record families.

ERDAT (Created Date)

For operations, created date on ERDAT indicates whether a row belongs in the current monitoring pass versus historical noise.

ERUHR (Created Time)

Explains why two monitoring passes differ: only the pass with stricter created time on ERUHR surfaces the disputed rows.

EVENT (Event)

Captures edge cases where event (EVENT) must be non-default to reproduce a customer-specific monitoring scenario.

FORFB (Requested Status)

Ensures reporting respects requested status constraints carried by FORFB.

KAPPL (Application)

Uses application from the source context so only records with KAPPL inside declared values proceed.

KSCHL (Mess Type)

Reduces false positives during peak windows by tightening mess type through KSCHL alongside state filters.

LDEST (Spool: Output device)

Stabilizes week-over-week metrics by fixing spool: output device (LDEST) while allowing duration thresholds to move.

MANUE (Message manually)

When tightened, message manually (MANUE) removes rows that would otherwise dilute attention from failing or stuck cases.

NACHA (Message transmission medium)

Gives auditors traceable criteria because message transmission medium on NACHA is applied consistently before any alert flag is raised.

NAUTO (Automatic)

For operations, automatic on NAUTO indicates whether a row belongs in the current monitoring pass versus historical noise.

OBJAC (Not used)

Improves readability of exported lists because not used (OBJAC) columns stay aligned with the configured filter intent.

OBJCP (Not used)

Narrows retrieved rows where not used (OBJCP) must match the configured selection for this monitor.

OBJKY (Object Key)

When left open per framework rules, OBJKY does not restrict object key; when set, only matching rows remain.

OBJPRI (Not used)

Reduces false positives during peak windows by tightening not used through OBJPRI alongside state filters.

OBJRN (Not used)

When left open per framework rules, OBJRN does not restrict not used; when set, only matching rows remain.

OBJRR (Not used)

When combined with destination discipline, not used on OBJRR keeps both breadth and depth of the extract intentional.

OBJSNS (Not used)

For operations, not used on OBJSNS indicates whether a row belongs in the current monitoring pass versus historical noise.

OBJTYPE (Object type)

Interprets object type as part of the selection contract: open ranges follow framework defaults; restricted ranges apply strict matching on OBJTYPE.

OPTARCNR (Archive number)

Supports escalation where archive number on OPTARCNR signals ownership for follow-up between Basis and functional teams.

OUTFL (Not used)

Helps distinguish technical versus business attributes when not used on OUTFL correlates with counters or status fields.

PARNR (Mess Partner)

Explains why two monitoring passes differ: only the pass with stricter mess partner on PARNR surfaces the disputed rows.

PARVW (Partner Role)

For distributed landscapes, partner role on PARVW often anchors which application server or destination appears in results.

PFLD1 - PFLD5 (Not used / SAPscript Form / Layout module for print formatting)

Treat PFLD1 through PFLD5 as the print-form and layout parameter family: configure the slots that matter for your scenario so SAPscript form and layout selections stay consistent with the message lines you expect in the extract.

PMID1 - PMID5 (Not used)

Configure PMID1 through PMID5 as reserved message-parameter slots: leave them open when your scenario does not use them, or populate the subset your installation maps so the monitor’s selection contract matches how output determination fills those positions.

PRIFB (Report Status by Mail)

For distributed landscapes, report status by mail on PRIFB often anchors which application server or destination appears in results.

REPET (Repeatability)

Stabilizes week-over-week metrics by fixing repeatability (REPET) while allowing duration thresholds to move.

REPID (Not used)

For operations, not used on REPID indicates whether a row belongs in the current monitoring pass versus historical noise.

SNDBC (Address Type)

Improves readability of exported lists because address type (SNDBC) columns stay aligned with the configured filter intent.

SNDCP (Not used)

Allows phased rollout: first widen SNDCP for not used, then tighten thresholds once baseline noise is understood.

SNDDR (Send as disregard)

When combined with destination discipline, send as disregard on SNDDR keeps both breadth and depth of the extract intentional.

SNDEX (Not used)

Supports operational control by evaluating not used through SNDEX for each candidate record.

SORT1 - SORT3 (Sort criterion)

SORT1, SORT2, and SORT3 define multi-level sort keys for the outbound list so reviewers see message rows in a stable business order (for example by partner, object, and time) before duration and state-style trimming.

SPRAS (Language)

Reduces false positives during peak windows by tightening language through SPRAS alongside state filters.

STATE_COLOR (State Color)

Filters lines by the derived color bucket used for severity-style triage in the monitor framework.

STATE_COLOR Options:

·        R — Red (error or failed-style outcomes).

·        G — Green (successful outcomes).

·        Y — Yellow (warning or in-process outcomes).

·        Additional literals may exist where the framework extends the palette for neutral states.

STATE_ICON (State Icon)

When left open per framework rules, STATE_ICON does not restrict state icon; when set, only matching rows remain.

STATUS_DESC (Short text)

Aligns exception volume with the chosen scope by testing short text via STATUS_DESC before alert evaluation.

TCODE (Communication strategy)

For operations, communication strategy on TCODE indicates whether a row belongs in the current monitoring pass versus historical noise.

TDARMOD (Storage Mode)

Aligns exception volume with the chosen scope by testing storage mode via TDARMOD before alert evaluation.

TDAUTORITY (Authorization)

Aligns exception volume with the chosen scope by testing authorization via TDAUTORITY before alert evaluation.

TDCOVTITLE (Cover Page Text)

Valuable when comparing health before and after a release—hold cover page text on TDCOVTITLE constant while varying other filters.

TDDIVISION (Department on Cover Page)

Explains why two monitoring passes differ: only the pass with stricter department on cover page on TDDIVISION surfaces the disputed rows.

TDID (Text ID)

Stabilizes week-over-week metrics by fixing text id (TDID) while allowing duration thresholds to move.

TDNAME (Object key)

Ensures reporting respects object key constraints carried by TDNAME.

TDOCOVER (SAP cover page)

After data is read, lines are removed unless sap cover page on TDOCOVER still satisfies the active multivalued selection.

TDRECEIVER (Recipient)

Supports operational control by evaluating recipient through TDRECEIVER for each candidate record.

TDSCHEDULE (Send time)

Captures edge cases where send time (TDSCHEDULE) must be non-default to reproduce a customer-specific monitoring scenario.

TDSPRAS (Language Key)

Mirrors how administrators slice operational lists: language key (TDSPRAS) is one lever that shapes which rows are comparable run over run.

TELFX (Fax number)

When tightened, fax number (TELFX) removes rows that would otherwise dilute attention from failing or stuck cases.

TELTX (Teletex number)

Improves readability of exported lists because teletex number (TELTX) columns stay aligned with the configured filter intent.

TELX1 (Telex number)

Connects to alert semantics: rows removed for failing telex number on TELX1 never reach downstream filtering.

TLAND (Country Key)

For distributed landscapes, country key on TLAND often anchors which application server or destination appears in results.

UHRVR (Processing time)

Ensures reporting respects processing time constraints carried by UHRVR.

USER_FLD (Dynamic Recipient User Field)

The USER_FLD parameter serves a dual purpose in work process monitoring: filtering results and enabling dynamic notification routing through the Dynamic Recipient List (DRL) mechanism. Instead of notifying pre-assigned users, the DRL mechanism provides a flexible, role-based notification system that automatically determines the appropriate recipients based on the monitoring results.

How DRL Works:

When USER_FLD is specified, the system extracts values from that field in the monitoring result set

These extracted values are then used as recipient addresses for alert notifications

This creates a dynamic, role-based notification mechanism that adapts based on the actual data being monitored

The mechanism is much more flexible than defining and constantly updating lists of specific pre-assigned users

USNAM (User name)

When harmonized with related filters, user name on USNAM isolates the highest-risk record families.

USRNAM (Company Address)

Helps distinguish technical versus business attributes when company address on USRNAM correlates with counters or status fields.

VSDAT (Send date)

Guards against oversized extracts when send date on VSDAT is narrowed together with client, user, or session filters.

VSTAT (Processing status of message)

Separates cross-client noise from in-scope work when processing status of message on VSTAT correlates with client or user attributes.

VSURA (Dispatch time from)

For distributed landscapes, dispatch time from on VSURA often anchors which application server or destination appears in results.

VSURB (Send time to)

Helps distinguish technical versus business attributes when send time to on VSURB correlates with counters or status fields.

VSZTP (Dispatch time)

Helps monitoring stay readable by requiring dispatch time (VSZTP) to match organizational or technical selectors when set.


### Parameter Relationships

How parameter combinations work together

DATUM supplies an explicit monitoring-date range when you populate it, so the population of message rows is anchored to calendar bounds you choose instead of a relative lookback alone.

When DATUM is not provided, BACKDAYS is the fallback that builds the lower monitoring date from the evaluation day backward, and that same window is then aligned with creation-date style selection on the message side unless you override the explicit creation-date filters separately.

DURATION and DURATION_UNIT act as an additional filter after date-oriented selection: only rows whose elapsed time from the reference creation timestamp to the evaluation moment still fit the configured duration band remain in the extract.

Both the date criteria (explicit DATUM or BACKDAYS-driven window, together with creation-date filters when you set them) and the DURATION / DURATION_UNIT age test are applied together—rows must satisfy the date side and the duration side before downstream presentation and alerting logic runs.

Application and object keys such as KAPPL, OBJKY, and KSCHL define which message families enter the dataset; narrowing them reduces cross-topic noise before time and aging filters run.

STATE_COLOR and related presentation attributes should be read together with queue or status-style fields so operators interpret severity-style buckets alongside the underlying processing state.


### Default Values

·        BACKDAYS - initial - treated as 1 by code (SY-DATUM minus one day as the lower bound for the monitoring date range when neither explicit monitoring dates nor a populated back-day interval fill the range tables).

·        DURATION - initial - treated as unconstrained by code (empty multivalued interval keeps every computed age value until explicit bounds are supplied on the selection interface).

·        DURATION_UNIT - initial - treated as M by code (minute-based duration math runs before the single-value unit read completes unless the caller overrides the unit afterward).


### Practical Example of Parameter Configuration

Use Case 1: Explicit calendar window and aging cap

Purpose: Anchor monitoring to a fixed fiscal-year window for message rows and cap how old they may be in minutes.

DATUM = 20250101 - 20251231
 DURATION = 240
 DURATION_UNIT = M
 KAPPL = V1
 KSCHL = BA00



Use Case 2: Relative lookback when explicit dates are not set

Purpose: Use default lookback behavior while still scoping to one output application and object key pattern.

BACKDAYS = 14
 KAPPL = V2
 OBJKY = 00*
 SPRAS = E



Use Case 3: State presentation with output type slice

Purpose: Focus on error-style presentation while limiting to one output type and partner view.

STATE_COLOR = R
 KSCHL = ZINV
 PARVW = WE
 SPRAS = E



Use Case 4: Duration in full-day units for batch-style review

Purpose: Flag rows older than several full days for a known condition type after date selection has already applied.

DURATION = 5
 DURATION_UNIT = F
 KSCHL = BA00
 SPRAS = E
 KAPPL = V1




## EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
| /SKN/S_SW_01_02_MSG_STATUS | ACALL | Unused field | CHAR(1) | NA_OBS001 |
| /SKN/S_SW_01_02_MSG_STATUS | ADRNR | Address number | CHAR(10) | AD_ADDRNUM |
| /SKN/S_SW_01_02_MSG_STATUS | AENDE | Change message flag | CHAR(1) | NA_AENDE |
| /SKN/S_SW_01_02_MSG_STATUS | AKTIV | Active | CHAR(1) | NA_AKTIV |
| /SKN/S_SW_01_02_MSG_STATUS | ANZAL | Number of messages (original + copies) | DEC(2) | NA_ANZAL |
| /SKN/S_SW_01_02_MSG_STATUS | CMFPNR | Error management number | NUMC(12) | NA_CMF_NR |
| /SKN/S_SW_01_02_MSG_STATUS | DATRE | Date of the expected response to a message | DATS(8) | NA_DATRE |
| /SKN/S_SW_01_02_MSG_STATUS | DATVR | Processing date | DATS(8) | NA_DATVR |
| /SKN/S_SW_01_02_MSG_STATUS | DELET | Release after output | CHAR(1) | TDDELETE |
| /SKN/S_SW_01_02_MSG_STATUS | DIMME | Print immediately | CHAR(1) | TDIMMED |
| /SKN/S_SW_01_02_MSG_STATUS | DSNAM | Spool request: Name | CHAR(6) | RSPO0NAME |
| /SKN/S_SW_01_02_MSG_STATUS | DSUF1 | Spool request: Suffix 1 | CHAR(4) | RSPO1NAME |
| /SKN/S_SW_01_02_MSG_STATUS | DSUF2 | Spool request: Suffix 2 | CHAR(12) | RSPO2NAME |
| /SKN/S_SW_01_02_MSG_STATUS | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_02_MSG_STATUS | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_02_MSG_STATUS | EDIKEY | Unused field | CHAR(8) | NA_OBS008 |
| /SKN/S_SW_01_02_MSG_STATUS | EDITRANS | Unused field | CHAR(6) | NA_OBS006 |
| /SKN/S_SW_01_02_MSG_STATUS | ERDAT | Date on which status record was created | DATS(8) | NA_ERDAT |
| /SKN/S_SW_01_02_MSG_STATUS | ERUHR | Time at which status record was created | TIMS(6) | NA_ERUHR |
| /SKN/S_SW_01_02_MSG_STATUS | EVENT | Event | CHAR(32) | SWO_EVENT |
| /SKN/S_SW_01_02_MSG_STATUS | FORFB | Requested Status | CHAR(1) | BCS_RQST |
| /SKN/S_SW_01_02_MSG_STATUS | KAPPL | Application for message conditions | CHAR(2) | SNA_KAPPL |
| /SKN/S_SW_01_02_MSG_STATUS | KSCHL | Message type | CHAR(4) | SNA_KSCHL |
| /SKN/S_SW_01_02_MSG_STATUS | LDEST | Spool: Output device | CHAR(4) | RSPOPNAME |
| /SKN/S_SW_01_02_MSG_STATUS | MANUE | Message processed manually | CHAR(1) | NA_MANUE |
| /SKN/S_SW_01_02_MSG_STATUS | NACHA | Message transmission medium | CHAR(1) | NA_NACHA |
| /SKN/S_SW_01_02_MSG_STATUS | NAUTO | Message determined via conditions | CHAR(1) | NA_NAUTO |
| /SKN/S_SW_01_02_MSG_STATUS | OBJAC | Unused field | CHAR(1) | NA_OBS001 |
| /SKN/S_SW_01_02_MSG_STATUS | OBJCP | Unused field | CHAR(1) | NA_OBS001 |
| /SKN/S_SW_01_02_MSG_STATUS | OBJKY | Object key | CHAR(30) | NA_OBJKEY |
| /SKN/S_SW_01_02_MSG_STATUS | OBJPRI | Unused field | CHAR(1) | NA_OBS001 |
| /SKN/S_SW_01_02_MSG_STATUS | OBJRN | Unused field | CHAR(1) | NA_OBS001 |
| /SKN/S_SW_01_02_MSG_STATUS | OBJRR | Unused field | CHAR(1) | NA_OBS001 |
| /SKN/S_SW_01_02_MSG_STATUS | OBJSNS | Unused field | CHAR(1) | NA_OBS001 |
| /SKN/S_SW_01_02_MSG_STATUS | OBJTYPE | Object type | CHAR(10) | OJ_NAME |
| /SKN/S_SW_01_02_MSG_STATUS | OPTARCNR | Archive number for optical archives | NUMC(10) | NA_OPTAR |
| /SKN/S_SW_01_02_MSG_STATUS | OUTFL | Unused field | CHAR(1) | NA_OBS001 |
| /SKN/S_SW_01_02_MSG_STATUS | PARNR | Message partner | CHAR(10) | NA_PARNR |
| /SKN/S_SW_01_02_MSG_STATUS | PARVW | Partner function (for example SH for ship-to party) | CHAR(2) | SNA_PARVW |
| /SKN/S_SW_01_02_MSG_STATUS | PFLD1 | Unused field | CHAR(30) | NA_OBS030 |
| /SKN/S_SW_01_02_MSG_STATUS | PFLD2 | Unused field | CHAR(30) | NA_OBS030 |
| /SKN/S_SW_01_02_MSG_STATUS | PFLD3 | Unused field | CHAR(30) | NA_OBS030 |
| /SKN/S_SW_01_02_MSG_STATUS | PFLD4 | WFMC: SAPscript form in 30-character field | CHAR(30) | NA_TDFORM |
| /SKN/S_SW_01_02_MSG_STATUS | PFLD5 | WFMC: Function module for print formatting | CHAR(30) | NA_LABEL |
| /SKN/S_SW_01_02_MSG_STATUS | PMID1 | Unused field | CHAR(3) | NA_OBS003 |
| /SKN/S_SW_01_02_MSG_STATUS | PMID2 | Unused field | CHAR(3) | NA_OBS003 |
| /SKN/S_SW_01_02_MSG_STATUS | PMID3 | Unused field | CHAR(3) | NA_OBS003 |
| /SKN/S_SW_01_02_MSG_STATUS | PMID4 | Unused field | CHAR(3) | NA_OBS003 |
| /SKN/S_SW_01_02_MSG_STATUS | PMID5 | Unused field | CHAR(3) | NA_OBS003 |
| /SKN/S_SW_01_02_MSG_STATUS | PRIFB | Setting for Which Statuses Are Reported by Mail | CHAR(1) | BCS_STML |
| /SKN/S_SW_01_02_MSG_STATUS | REPET | Repeatability of message | CHAR(1) | NA_REPET |
| /SKN/S_SW_01_02_MSG_STATUS | REPID | Unused field | CHAR(8) | NA_OBS008 |
| /SKN/S_SW_01_02_MSG_STATUS | SNDBC | Address type (1=Organization, 2=Person, 3=Contact person) | CHAR(1) | AD_ADRTYPE |
| /SKN/S_SW_01_02_MSG_STATUS | SNDCP | Unused field | CHAR(1) | NA_OBS001 |
| /SKN/S_SW_01_02_MSG_STATUS | SNDDR | Send: As disregard | CHAR(1) | SO_SND_DR |
| /SKN/S_SW_01_02_MSG_STATUS | SNDEX | Unused field | CHAR(1) | NA_OBS001 |
| /SKN/S_SW_01_02_MSG_STATUS | SORT1 | Sort criteria for message status records | CHAR(20) | NAST_SORTK |
| /SKN/S_SW_01_02_MSG_STATUS | SORT2 | Sort criteria for message status records | CHAR(20) | NAST_SORTK |
| /SKN/S_SW_01_02_MSG_STATUS | SORT3 | Sort criteria for message status records | CHAR(20) | NAST_SORTK |
| /SKN/S_SW_01_02_MSG_STATUS | SPRAS | Message language | LANG(1) | NA_SPRAS |
| /SKN/S_SW_01_02_MSG_STATUS | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_02_MSG_STATUS | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_02_MSG_STATUS | STATUS_DESC | Short description of object | CHAR(60) | EDI_TEXT60 |
| /SKN/S_SW_01_02_MSG_STATUS | TCODE | Communication strategy | CHAR(4) | CSTRATEGY |
| /SKN/S_SW_01_02_MSG_STATUS | TDARMOD | Print: Archiving mode | CHAR(1) | SYARMOD |
| /SKN/S_SW_01_02_MSG_STATUS | TDAUTORITY | Print: Authorization | CHAR(12) | SYPRBER |
| /SKN/S_SW_01_02_MSG_STATUS | TDCOVTITLE | Spool Description | CHAR(68) | SYPRTXT |
| /SKN/S_SW_01_02_MSG_STATUS | TDDIVISION | Spool Department Name | CHAR(12) | SYPRABT |
| /SKN/S_SW_01_02_MSG_STATUS | TDID | Text ID | CHAR(4) | TDID |
| /SKN/S_SW_01_02_MSG_STATUS | TDNAME | Object key (long) | CHAR(70) | NA_OBJKY_L |
| /SKN/S_SW_01_02_MSG_STATUS | TDOCOVER | Print: SAP cover page | CHAR(1) | SYPRSAP |
| /SKN/S_SW_01_02_MSG_STATUS | TDRECEIVER | Spool Recipient Name | CHAR(12) | SYPRREC |
| /SKN/S_SW_01_02_MSG_STATUS | TDSCHEDULE | Send time request | CHAR(3) | SKSCHEDULE |
| /SKN/S_SW_01_02_MSG_STATUS | TDSPRAS | Language Key | LANG(1) | SPRAS |
| /SKN/S_SW_01_02_MSG_STATUS | TELFX | Fax number | CHAR(31) | NA_TELFX |
| /SKN/S_SW_01_02_MSG_STATUS | TELTX | Teletex number | CHAR(30) | TELTX |
| /SKN/S_SW_01_02_MSG_STATUS | TELX1 | Telex number | CHAR(30) | TELX1 |
| /SKN/S_SW_01_02_MSG_STATUS | TLAND | Country Key | CHAR(3) | LAND1 |
| /SKN/S_SW_01_02_MSG_STATUS | UHRVR | Processing time | TIMS(6) | NA_UHRVR |
| /SKN/S_SW_01_02_MSG_STATUS | USNAM | User name | CHAR(12) | USNAM |
| /SKN/S_SW_01_02_MSG_STATUS | USRNAM | WFMC: Address number, 12 characters | CHAR(12) | NA_PERS_12 |
| /SKN/S_SW_01_02_MSG_STATUS | VSDAT | Requested date for sending message | DATS(8) | NA_VSDAT |
| /SKN/S_SW_01_02_MSG_STATUS | VSTAT | Processing status of message | CHAR(1) | NA_VSTAT |
| /SKN/S_SW_01_02_MSG_STATUS | VSURA | Requested time for sending message (from) | TIMS(6) | NA_VSURA |
| /SKN/S_SW_01_02_MSG_STATUS | VSURB | Requested time for sending message (to) | TIMS(6) | NA_VSURB |
| /SKN/S_SW_01_02_MSG_STATUS | VSZTP | Dispatch time | CHAR(1) | NA_VSZTP |


## ABAP Code


```abap
FUNCTION /SKN/F_SW_01_02_MSG_STATUS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_MSG_STATUS OPTIONAL
*"----------------------------------------------------------------------
RANGES : R_KAPPL FOR NAST-KAPPL ,
         R_STATE_COLOR FOR /SKN/S_SW_SYS_JOB-STATE_COLOR,
         R_OBJKY FOR NAST-OBJKY,
         R_KSCHL FOR NAST-KSCHL,
         R_SPRAS FOR NAST-SPRAS,
         R_PARNR FOR NAST-PARNR,
         R_PARVW FOR NAST-PARVW,
         R_ERDAT FOR NAST-ERDAT,
         R_ERUHR FOR NAST-ERUHR,
         R_VSTAT FOR NAST-VSTAT,
         R_NACHA FOR NAST-NACHA,
         R_VSZTP FOR NAST-VSZTP,
         R_USNAM FOR NAST-USNAM,
         R_TCODE FOR NAST-TCODE,
         R_LDEST FOR NAST-LDEST,
         R_TDID  FOR NAST-TDID,
         R_OBJTYPE FOR NAST-OBJTYPE,
         R_DATUM   FOR SY-DATUM .
DATA :   IS_GENERAL(1) TYPE C.
DATA : DATE_FROM LIKE SY-DATUM ,
       BACKDAYS  TYPE I .
DATA : SY_TABIX LIKE SY-TABIX .
DATA : LANGU TYPE LANGU .
DATA_MULTY: DURATION   /SKN/E_SW_DURATION.
DATA_SINGLE: DURATION_UNIT  /SKN/E_SW_DURATION_UNIT.
DATA :   SY_DATLO LIKE SY-DATLO ,
         SY_TIMLO LIKE SY-TIMLO .
DATA : TIME_DIFF TYPE  INT4 .
*-- Fill Selection Option Tables
   LOOP AT T_SELECT WHERE FIELDNM = 'OBJKY'.
     MOVE-CORRESPONDING T_SELECT TO R_OBJKY.
     APPEND R_OBJKY.
   ENDLOOP.
  LOOP AT T_SELECT WHERE FIELDNM = 'KAPPL'.
    MOVE-CORRESPONDING T_SELECT TO R_KAPPL.
    APPEND R_KAPPL.
  ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'STATE_COLOR'.
     MOVE-CORRESPONDING T_SELECT TO R_STATE_COLOR.
     APPEND R_STATE_COLOR.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'KSCHL'.
     MOVE-CORRESPONDING T_SELECT TO R_KSCHL.
     APPEND R_KSCHL.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'SPRAS'.
     MOVE-CORRESPONDING T_SELECT TO R_SPRAS.
     APPEND R_SPRAS.
   ENDLOOP.
*
   LOOP AT T_SELECT WHERE FIELDNM = 'PARNR'.
     MOVE-CORRESPONDING T_SELECT TO R_PARNR.
     APPEND R_PARNR.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'PARVW'.
     MOVE-CORRESPONDING T_SELECT TO R_PARVW.
     APPEND R_PARVW.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ERDAT'.
     MOVE-CORRESPONDING T_SELECT TO R_ERDAT.
     APPEND R_ERDAT.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'ERUHR'.
     MOVE-CORRESPONDING T_SELECT TO R_ERUHR.
     APPEND R_ERUHR.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'VSTAT'.
     MOVE-CORRESPONDING T_SELECT TO R_VSTAT.
     APPEND R_VSTAT.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'NACHA'.
     MOVE-CORRESPONDING T_SELECT TO R_NACHA.
     APPEND R_NACHA.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'VSZTP'.
     MOVE-CORRESPONDING T_SELECT TO R_VSZTP.
     APPEND R_VSZTP.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'USNAM'.
     MOVE-CORRESPONDING T_SELECT TO R_USNAM.
     APPEND R_USNAM.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'TCODE'.
     MOVE-CORRESPONDING T_SELECT TO R_TCODE.
     APPEND R_TCODE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'LDEST'.
     MOVE-CORRESPONDING T_SELECT TO R_LDEST.
     APPEND R_LDEST.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'TDID'.
     MOVE-CORRESPONDING T_SELECT TO R_TDID.
     APPEND R_TDID.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'OBJTYPE'.
     MOVE-CORRESPONDING T_SELECT TO R_OBJTYPE.
     APPEND R_OBJTYPE.
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'IS_GENERAL'.
     IF NOT T_SELECT-LOW IS INITIAL.
       IS_GENERAL = 'X'.
     ENDIF.
     EXIT .
   ENDLOOP.
   LOOP AT T_SELECT WHERE FIELDNM = 'DATUM'.
     MOVE-CORRESPONDING T_SELECT TO R_DATUM.
     APPEND R_DATUM.
   ENDLOOP.
   IF R_DATUM[] IS INITIAL .
     LOOP AT T_SELECT WHERE FIELDNM = 'BACKDAYS'.
       R_DATUM-SIGN = 'I' .
        R_DATUM-OPTION = 'GE' .
         BACKDAYS = T_SELECT-LOW .
         DATE_FROM = SY-DATUM - BACKDAYS .
         R_DATUM-LOW = DATE_FROM .
         APPEND R_DATUM.
         EXIT.
     ENDLOOP.
     IF R_DATUM[] IS INITIAL .
       R_DATUM-SIGN = 'I' .
        R_DATUM-OPTION = 'GE' .
         BACKDAYS = 1 .
         DATE_FROM = SY-DATUM - BACKDAYS .
         R_DATUM-LOW = DATE_FROM .
       APPEND R_DATUM.
     ENDIF .
   ENDIF.
  IF R_ERDAT[] IS INITIAL.
    R_ERDAT[] = R_DATUM[] .
  ENDIF.
  "-----
  LANGU = SY-LANGU.
  READ TABLE R_SPRAS INDEX 1.
  IF SY-SUBRC IS INITIAL.
    LANGU = R_SPRAS-LOW.
  ENDIF.
  SELECT_MULTY: DURATION.
  LV_DURATION_UNIT = 'M'.
  SELECT_SINGLE: DURATION_UNIT.
  "--- Run Cloud Mode -----
  DATA_SINGLE: SW_DEST RFCDEST.             .
  SELECT_SINGLE: SW_DEST.
  IF LV_SW_DEST IS NOT INITIAL.
    CALL FUNCTION '/SKN/FC_SW_01_02_MSG_STATUS'
      IMPORTING
        IS_ALERT       = IS_ALERT
      TABLES
        T_SELECT       = T_SELECT
        T_DATA         = T_DATA.
  ENDIF.
  CHECK LV_SW_DEST IS INITIAL.
"--- Run Cloud Mode -----
  SY_DATLO = SY-DATUM .        " Appl Server's Date
  SY_TIMLO = SY-UZEIT.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
   SELECT *
      FROM NAST
      INTO CORRESPONDING FIELDS OF TABLE T_DATA
      WHERE KAPPL IN R_KAPPL
        AND OBJKY IN R_OBJKY
        AND KSCHL IN R_KSCHL
        AND SPRAS IN R_SPRAS
        AND PARNR IN R_PARNR
        AND PARVW IN R_PARVW
        AND ERDAT IN R_ERDAT
        AND ERUHR IN R_ERUHR
        AND VSTAT IN R_VSTAT
        AND NACHA IN R_NACHA
        AND VSZTP IN R_VSZTP
        AND USNAM IN R_USNAM
        AND TCODE IN R_TCODE
        AND LDEST IN R_LDEST
        AND TDID IN R_TDID
        AND OBJTYPE IN R_OBJTYPE.
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
     CALL FUNCTION '/SKN/F_SW_01_02_MSG_VSTAT'
       EXPORTING
         VSTAT             = T_DATA-VSTAT
         LANGU             = LANGU
       IMPORTING
         STATUS_DESC       = T_DATA-STATUS_DESC
         STATE_COLOR       = T_DATA-STATE_COLOR .
*
     CALL FUNCTION '/SKN/F_SW_GET_STATE_ICON'
       EXPORTING
         STATE_COLOR       = T_DATA-STATE_COLOR
       IMPORTING
         STATE_ICON        = T_DATA-STATE_ICON         .
     MODIFY T_DATA INDEX SY_TABIX.
  ENDLOOP.
  LOOP AT T_DATA .
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-STATE_COLOR IN R_STATE_COLOR.
      DELETE T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-ERDAT
          T_FROM            = T_DATA-ERUHR
          D_TO              = SY_DATLO
          T_TO              = SY_TIMLO
          TIME_UNIT         = LV_DURATION_UNIT
        IMPORTING
          TIME_DIFF         = TIME_DIFF
        EXCEPTIONS
          WRONG_VALUE       = 1
          OTHERS            = 2    .
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
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
