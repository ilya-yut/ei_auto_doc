# Exception Indicator: SAPconnect Send Requests Monitoring (SW_01_02_SOST)


## General Overview

This Exception Indicator (EI) provides comprehensive monitoring and analysis of SAP SAPconnect send requests to ensure reliable email, fax, and other communication processing through the SAP system. SAPconnect technology enables automated communication delivery from SAP applications to external recipients via various communication channels, making it essential for business-critical notification scenarios where communication reliability and processing performance must be ensured.

SAPconnect (SOST) extends the standard SAP communication framework by providing transactional communication processing with status tracking and delivery confirmation. The technology ensures that outbound communications are processed reliably and provides detailed status information for troubleshooting and monitoring purposes, crucial for business processes that depend on automated notifications and communications.

This Exception Indicator provides detailed SAPconnect monitoring capabilities that enable:

Send request tracking to monitor the status and performance of outbound communication processing

Communication status analysis for understanding delivery patterns and identifying failed operations

Address resolution monitoring to track recipient address processing and validation

Processing duration assessment for identifying communication delays and system bottlenecks

Message type analysis for detailed tracking of different communication channels and formats

The monitoring solution analyzes SAPconnect statistics from the SOST framework using the SX_SNDREC_SELECT function, similar to data available through the SOST transaction (SAPconnect Administration), and provides enhanced filtering capabilities to focus on specific communication types, statuses, recipients, and time periods. This enables targeted analysis of communication performance characteristics and identification of delivery-related issues.

This Exception Indicator checks whether SAP SAPconnect processing is functioning efficiently and identifies potential issues that may impact automated communication delivery, notification processing, and business communication workflows.


## Problem Description

Poor SAPconnect performance and processing failures indicate communication system issues causing:

Communication and Delivery Problems

Failed outbound communications preventing critical business notifications and automated messaging

Stuck send requests causing communication delays and recipient notification failures

Address resolution errors leading to delivery failures and communication processing issues

Communication channel failures disrupting automated business workflows and notification processes

System Performance Issues

Resource contention from excessive communication processing workload affecting overall system performance

Memory consumption issues from accumulated failed communications impacting available system resources

Processing delays causing bottlenecks in time-critical business communications and automated notifications

Database locking issues from long-running communication transactions affecting system responsiveness

Business Impact

Communication delivery failures due to incomplete or failed SAPconnect processing affecting business operations

SLA violations from communication delays impacting service level commitments and operational efficiency

Business process interruptions from unreliable communication delivery affecting operational continuity

Potential message loss or delivery corruption from SAPconnect processing failures compromising business communication integrity


## Suggested Resolution

Immediate Response

Investigate failed and stuck SAPconnect entries using SOST transaction for detailed error analysis and resolution

Check system resource utilization and memory consumption during communication processing periods for capacity assessment

Review SAPconnect error logs and system messages for root cause identification and pattern analysis

Analyze communication delivery patterns and identify frequently failing operations for targeted remediation

System Assessment

Monitor SAPconnect processing performance trends and delivery completion patterns for optimization opportunities

Evaluate system sizing and resource allocation for communication processing workload management

Check network connectivity and communication channel configuration for reliability optimization

Analyze communication processing patterns by message type, recipient, and system for performance tuning

Corrective Actions

Optimize SAPconnect processing parameters and system configurations for improved reliability and performance

Implement communication error handling and retry mechanisms for enhanced fault tolerance and recovery

Establish proactive SAPconnect monitoring and alerting procedures for early issue detection and prevention

Plan system capacity upgrades based on communication volume analysis and processing requirements


## Parameters


### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control data retrieval and processing.


| # | Field | Description | Type | Length | Decimal | Data Element | Domain |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 1 | ADDRESS | Address name | CHAR | 35 | 0 | SO_ADRNAM | SO_STRI035 |
| 2 | ADRNR | Address number | CHAR | 10 | 0 | SO_ADR_NR | ADRNR |
| 3 | ATTLEN | Size of all attachms | CHAR | 12 | 0 | SO_ATT_LEN | SO_OBJ_LEN |
| 4 | BACKDAYS | Backdays |  | 0 | 0 |  |  |
| 5 | COLOR | Not More Closely Defined Area, Possibly Used for Patchlevels | CHAR | 4 | 0 | CHAR4 | CHAR4 |
| 6 | COUNTER | Counter for status entries for this send process | CHAR | 5 | 0 | SO_STA_CNT | SO_NUMC005 |
| 7 | DEF_STAT | Boolean | CHAR | 1 | 0 | OS_BOOLEAN | OS_BOOLEAN |
| 8 | DURATION | Duration In Time Units | INT4 | 10 | 0 | /SKN/E_SW_DURATION |  |
| 9 | DURATION_UNIT | Duration Unit(D/H/M) | CHAR | 1 | 0 | /SKN/E_SW_DURATION_UNIT | /SKN/D_SW_DURATION_UNIT |
| 10 | FORNO | Number of forwarder | CHAR | 12 | 0 | SO_FOR_NO | SO_OBJ_NO |
| 11 | FORTP | Forwarder type | CHAR | 3 | 0 | SO_FOR_TP | SO_OBJ_TP |
| 12 | FORYR | Forwarder year | CHAR | 2 | 0 | SO_FOR_YR | SO_OBJ_YR |
| 13 | ICON | Carrier field for icons | CHAR | 132 | 0 | ICON_TEXT | SYCHAR132 |
| 14 | LANGU | Language for texts |  | 0 | 0 |  |  |
| 15 | LENGTH | Document size | CHAR | 12 | 0 | SO_OBJ_LEN | SO_OBJ_LEN |
| 16 | MANAGE_IN_UTC | 'X' - Manage in UTC |  | 0 | 0 |  |  |
| 17 | MARK | Boolean | CHAR | 1 | 0 | OS_BOOLEAN | OS_BOOLEAN |
| 18 | MAX_RECORDS | Max. Output Records |  | 0 | 0 |  |  |
| 19 | MESSAGE | Message text | CHAR | 220 | 0 | BAPI_MSG | TEXT220 |
| 20 | MSGID | Message Class | CHAR | 20 | 0 | SYMSGID | ARBGB |
| 21 | MSGNO | Message number | NUMC | 3 | 0 | SYMSGNO | SYMSGNO |
| 22 | MSGTP | Type of send request (message or status) | CHAR | 1 | 0 | SO_MSGTP | SO_MSGTP |
| 23 | MSGTXT | Text field lngth 255 | CHAR | 255 | 0 | SO_TEXT255 | SO_TEXT255 |
| 24 | MSGTY | Message Type | CHAR | 1 | 0 | SYMSGTY | SYCHAR01 |
| 25 | MSGV1 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 26 | MSGV2 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 27 | MSGV3 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 28 | MSGV4 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 29 | NODE | Node ID | CHAR | 6 | 0 | SX_NODE_ID | SX_NODE_ID |
| 30 | OBJLEN | Document size | CHAR | 12 | 0 | SO_OBJ_LEN | SO_OBJ_LEN |
| 31 | OBJNO | Object number | CHAR | 12 | 0 | SO_OBJ_NO | SO_OBJ_NO |
| 32 | OBJSNS | Sensitivity | CHAR | 1 | 0 | SO_OBJ_SNS | SO_OBJ_SNS |
| 33 | OBJTP | Document class | CHAR | 3 | 0 | SO_OBJ_TP | SO_OBJ_TP |
| 34 | OBJYR | Object year | CHAR | 2 | 0 | SO_OBJ_YR | SO_OBJ_YR |
| 35 | RC_NAME | Full Name | CHAR | 80 | 0 | AD_NAMTEXT | TEXT80 |
| 36 | RECNO | Recipient number | CHAR | 12 | 0 | SO_REC_NO | SO_OBJ_NO |
| 37 | RECTP | Recipient Type | CHAR | 3 | 0 | SO_REC_TP | SO_OBJ_TP |
| 38 | RECYR | Recipient year | CHAR | 2 | 0 | SO_REC_YR | SO_OBJ_YR |
| 39 | SCOMNO | Number of the SAPcomm ID | CHAR | 12 | 0 | SO_SCOM_NO | SO_OBJ_NO |
| 40 | SCOMTP | Type of SAPcomm ID | CHAR | 3 | 0 | SO_SCOM_TP | SO_OBJ_TP |
| 41 | SCOMYR | Year of SAPcomm ID | CHAR | 2 | 0 | SO_SCOM_YR | SO_OBJ_YR |
| 42 | SENDER | Address name | CHAR | 35 | 0 | SO_ADRNAM | SO_STRI035 |
| 43 | SNDART | AddrType | CHAR | 3 | 0 | SX_ADDRTYP | SX_ADDRTYP |
| 44 | SNDART_DESC | Short text | CHAR | 60 | 0 | DDTEXT | DDTEXT |
| 45 | SNDART_L | Text field length 50 | CHAR | 50 | 0 | SO_TEXT050 | SO_TEXT050 |
| 46 | SNDNO | Sender no. | CHAR | 12 | 0 | SO_SND_NO | SO_OBJ_NO |
| 47 | SNDREQ | GUID | RAW | 16 | 0 | OS_GUID | SYSUUID |
| 48 | SNDTP | Sender type | CHAR | 3 | 0 | SO_SND_TP | SO_OBJ_TP |
| 49 | SNDYR | Sender year | CHAR | 2 | 0 | SO_SND_YR | SO_OBJ_YR |
| 50 | SOST_MSGID | Message Class | CHAR | 20 | 0 | SYMSGID | ARBGB |
| 51 | SOST_MSGV1 | Message Variable | CHAR | 50 | 0 | SYMSGV | SYCHAR50 |
| 52 | STATE_COLOR | State Color | CHAR | 1 | 0 | /SKN/E_SW_STATE_COLOR | /SKN/D_SW_STATE_COLOR |
| 53 | STATE_ICON | State Icon | CHAR | 4 | 0 | /SKN/E_SW_STATE_ICON | ICON |
| 54 | STATUS | Status of the sent object | CHAR | 12 | 0 | SO_REC_STA | SO_TEXT012 |
| 55 | STATUS_DESC | SW Message | CHAR | 255 | 0 | /SKN/E_SW_TEXT | /SKN/D_SW_LTEXT |
| 56 | STAT_DATE | Date of status | DATS | 8 | 0 | SO_STADATE | DATUM |
| 57 | STAT_ERROR | 'X' - Errors |  | 0 | 0 |  |  |
| 58 | STAT_OK | 'X' - Sent |  | 0 | 0 |  |  |
| 59 | STAT_TIME | Time of status | TIMS | 6 | 0 | SO_STATIME | UZEIT |
| 60 | STAT_TRANSIT | 'X' - Transmitted |  | 0 | 0 |  |  |
| 61 | STAT_WAIT | 'X' - Waiting |  | 0 | 0 |  |  |
| 62 | TITEL | Document title | CHAR | 50 | 0 | SO_OBJ_DES | SO_OBJ_DES |
| 63 | USERNAM | Sender name | CHAR | 12 | 0 | SO_SND_NAM | USERNAME |
| 64 | USER_FLD | Dynamic Recipient User Field |  | 0 | 0 |  |  |
| 65 | WAIT_DATE | No processing before this date | DATS | 8 | 0 | SO_WAITDAT | DATUM |
| 66 | WAIT_TIME | No processing before this time | TIMS | 6 | 0 | SO_WAITTIM | UZEIT |


### Parameter Configuration Guidelines

IMPORTANT: This section provides configuration guidance for ALL 66 parameters listed in the Parameters Reference Table above.

ADDRESS (Address name):

ADDRESS applies address name as a selection/enrichment control in SOST monitoring, influencing which records are returned and how results are interpreted.

ADRNR (Address number):

ADRNR carries address number for actor/recipient traceability, enabling ownership-level investigation and communication accountability in exception analysis.

ATTLEN (Size of all attachms):

ATTLEN applies size of all attachms as a selection/enrichment control in SOST monitoring, influencing which records are returned and how results are interpreted.

BACKDAYS (Backdays):

BACKDAYS defines the historical monitoring window by specifying how many days backward from today to retrieve records. 0 – today, 1 – today + yesterday etc.

COLOR (Not More Closely Defined Area, Possibly Used for Patchlevels):

COLOR applies not more closely defined area, possibly used for patchlevels as a selection/enrichment control in SOST monitoring, influencing which records are returned and how results are interpreted.

COUNTER (Counter for status entries for this send process):

COUNTER applies counter for status entries for this send process as a selection/enrichment control in SOST monitoring, influencing which records are returned and how results are interpreted.

DEF_STAT (Boolean):

DEF_STAT applies boolean as a selection/enrichment control in SOST monitoring, influencing which records are returned and how results are interpreted.

DURATION (Duration In Time Units):

DURATION is the numeric aging threshold used after status timestamp comparison; it controls when an item is treated as overdue for exception focus.

DURATION_UNIT (Duration Unit(D/H/M)):

DURATION_UNIT defines the time unit for DURATION comparison, which directly changes whether thresholds are interpreted as minutes, hours, or days.

DURATION_UNIT Options:

·        H: Hours

·        M: Minutes

·        D: Days

·        F: Full days for specific day filtering

FORNO (Number of forwarder):

FORNO contributes number of forwarder to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

FORTP (Forwarder type):

FORTP contributes forwarder type to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

FORYR (Forwarder year):

FORYR contributes forwarder year to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

ICON (Carrier field for icons):

ICON applies carrier field for icons as a selection/enrichment control in SOST monitoring, influencing which records are returned and how results are interpreted.

LANGU (Language for texts):

LANGU applies language for texts as a selection/enrichment control in SOST monitoring, influencing which records are returned and how results are interpreted.

LENGTH (Document size):

LENGTH applies document size as a selection/enrichment control in SOST monitoring, influencing which records are returned and how results are interpreted.

MANAGE_IN_UTC ('X' - Manage in UTC):

MANAGE_IN_UTC controls whether current-time comparison is normalized to UTC or evaluated in local system time, which affects cross-time-zone consistency.

MANAGE_IN_UTC Options:

·        X: UTC mode for distributed/multi-time-zone landscapes.

·        (blank): Local/system time mode for single-time-zone operation.

MARK (Boolean):

MARK applies boolean as a selection/enrichment control in SOST monitoring, influencing which records are returned and how results are interpreted.

MAX_RECORDS (Max. Output Records):

MAX_RECORDS caps extracted result volume to keep runtime predictable while still returning the highest-priority exception population.

MESSAGE (Message text):

MESSAGE applies message text as a selection/enrichment control in SOST monitoring, influencing which records are returned and how results are interpreted.

MSGID (Message Class):

MSGID identifies the ABAP message class namespace used to resolve the semantic category of the communication status or error condition.

MSGNO (Message number):

MSGNO is the numeric message key within MSGID and pinpoints the exact cataloged message variant for root-cause identification.

MSGTP (Type of send request (message or status)):

MSGTP indicates whether the record context represents a send-request message versus status-oriented messaging semantics in SOST.

MSGTXT (Text field lngth 255):

MSGTXT contains the rendered long text payload (up to field length) that analysts read directly when diagnosing the communication event.

MSGTY (Message Type):

MSGTY is the one-character ABAP message severity type and helps classify the diagnostic intent (for example error versus warning/info).

MSGV1 - MSGV4 (Message Variables):

MSGV1 through MSGV4 form one ordered placeholder set used by message rendering calls; together they provide substitution values that complete final diagnostic text.

MSGV1 - MSGV4 Connection:

Treat these four fields as one logical group because meaning emerges from the combined position-based substitution, not from isolated single-field review.

NODE (Node ID):

NODE contributes node id to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

OBJLEN (Document size):

OBJLEN contributes document size to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

OBJNO (Object number):

OBJNO contributes object number to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

OBJSNS (Sensitivity):

OBJSNS contributes sensitivity to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

OBJTP (Document class):

OBJTP contributes document class to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

OBJYR (Object year):

OBJYR contributes object year to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

RC_NAME (Full Name):

RC_NAME carries full name for actor/recipient traceability, enabling ownership-level investigation and communication accountability in exception analysis.

RECNO (Recipient number):

RECNO contributes recipient number to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

RECTP (Recipient Type):

RECTP contributes recipient type to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

RECYR (Recipient year):

RECYR contributes recipient year to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

SCOMNO (Number of the SAPcomm ID):

SCOMNO contributes number of the sapcomm id to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

SCOMTP (Type of SAPcomm ID):

SCOMTP contributes type of sapcomm id to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

SCOMYR (Year of SAPcomm ID):

SCOMYR contributes year of sapcomm id to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

SENDER (Address name):

SENDER carries address name for actor/recipient traceability, enabling ownership-level investigation and communication accountability in exception analysis.

SNDART (AddrType):

SNDART contributes addrtype to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

SNDART_DESC (Short text):

SNDART_DESC contributes short text to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

SNDART_L (Text field length 50):

SNDART_L contributes text field length 50 to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

SNDNO (Sender no.):

SNDNO contributes sender no. to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

SNDREQ (GUID):

SNDREQ contributes guid to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

SNDTP (Sender type):

SNDTP contributes sender type to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

SNDYR (Sender year):

SNDYR contributes sender year to routing/object identity context so analysts can isolate exception patterns by transport path and object lineage.

SOST_MSGID (Message Class):

SOST_MSGID provides message class used in status/message interpretation, helping convert raw send-request records into actionable diagnostic context.

SOST_MSGV1 (Message Variable):

SOST_MSGV1 provides message variable used in status/message interpretation, helping convert raw send-request records into actionable diagnostic context.

STATE_COLOR (State Color):

STATE_COLOR is the visual severity/state classifier used after status mapping so analysts can triage exception rows quickly by color-coded risk level.

STATE_COLOR Options:

·        R: Red (error/failed outcomes).

·        G: Green (successful outcomes).

·        Y: Yellow (processing/warning outcomes).

·        B: Blue (waiting/queued outcomes).

·        Gray: Gray (retry/future outcomes).

Code does not enumerate all color constants inline in this function body, so standard SAPconnect mapping is used as documented fallback.

STATE_ICON (State Icon):

STATE_ICON provides state icon used in status/message interpretation, helping convert raw send-request records into actionable diagnostic context.

STATUS (Status of the sent object):

STATUS is the lifecycle state filter for SOST requests and is used to separate waiting, in-process, successful, and failed communication outcomes.

STATUS Options:

·        WAIT: Waiting in queue.

·        TRANSIT: Currently processed/transmitted.

·        OK: Successfully processed/delivered.

·        DIRECT: Processed directly.

·        ERROR: Processing error occurred.

·        INCONS: Inconsistent state/data condition.

·        RETRY: Scheduled retry after failure.

·        FUTURE: Scheduled for future processing.

·        ACTIVE: Active in processing lifecycle.

STATUS_DESC (SW Message):

STATUS_DESC provides sw message used in status/message interpretation, helping convert raw send-request records into actionable diagnostic context.

STAT_DATE (Date of status):

STAT_DATE uses date of status to constrain monitoring interval boundaries and ensure temporal filtering matches the intended operational review window.

STAT_ERROR ('X' - Errors):

STAT_ERROR is the explicit selector switch for ERROR state inclusion, typically used for remediation-oriented exception slices.

STAT_ERROR Options:

·        X: Include this status selector in filtering logic.

·        (blank): Do not explicitly force this selector.

STAT_OK ('X' - Sent):

STAT_OK is the explicit selector switch for OK state inclusion when successful deliveries must remain visible in control evidence.

STAT_OK Options:

·        X: Include this status selector in filtering logic.

·        (blank): Do not explicitly force this selector.

STAT_TIME (Time of status):

STAT_TIME uses time of status to constrain monitoring interval boundaries and ensure temporal filtering matches the intended operational review window.

STAT_TRANSIT ('X' - Transmitted):

STAT_TRANSIT is the explicit selector switch for TRANSIT state inclusion to monitor in-flight processing latency.

STAT_TRANSIT Options:

·        X: Include this status selector in filtering logic.

·        (blank): Do not explicitly force this selector.

STAT_WAIT ('X' - Waiting):

STAT_WAIT is the explicit selector switch for WAIT state inclusion, allowing queue-state-focused monitoring runs.

STAT_WAIT Options:

·        X: Include this status selector in filtering logic.

·        (blank): Do not explicitly force this selector.

TITEL (Document title):

TITEL applies document title as a selection/enrichment control in SOST monitoring, influencing which records are returned and how results are interpreted.

USERNAM (Sender name):

USERNAM carries sender name for actor/recipient traceability, enabling ownership-level investigation and communication accountability in exception analysis.

USER_FLD (Dynamic Recipient User Field):

The USER_FLD parameter serves a dual purpose in work process monitoring: filtering results and enabling dynamic notification routing through the Dynamic Recipient List (DRL) mechanism. Instead of notifying pre-assigned users, the DRL mechanism provides a flexible, role-based notification system that automatically determines the appropriate recipients based on the monitoring results.

How DRL Works:

When USER_FLD is specified, the system extracts values from that field in the monitoring result set

These extracted values are then used as recipient addresses for alert notifications

This creates a dynamic, role-based notification mechanism that adapts based on the actual data being monitored

The mechanism is much more flexible than defining and constantly updating lists of specific pre-assigned users

WAIT_DATE (No processing before this date):

WAIT_DATE uses no processing before this date to constrain monitoring interval boundaries and ensure temporal filtering matches the intended operational review window.

WAIT_TIME (No processing before this time):

WAIT_TIME uses no processing before this time to constrain monitoring interval boundaries and ensure temporal filtering matches the intended operational review window.


### Parameter Relationship

How parameter combinations work together

Date and Time Controls:

·        BACKDAYS is the fallback date window. If explicit dates are not provided, the system uses BACKDAYS to build the initial lookback range.

·        DATUM / SND_DATE are explicit date filters. When these are provided, they define the date window directly and override fallback behavior.

·        SND_TIME, STAT_TIME, WAIT_TIME, and MANAGE_IN_UTC refine timestamp handling inside the selected date window.

Status and Outcome Controls:

·        STAT_* selectors define which process states are emphasized.

·        STATUS, STATE_COLOR, and STATE_ICON represent interpretable processing outcomes.

Duration Prioritization:

·        DURATION + DURATION_UNIT are an additional age filter after date selection.

·        Simple flow: first filter by date window, then filter by record age (duration).

·        Final result keeps records that satisfy both date conditions and duration conditions.


### Practical Configuration Examples

Use Case 1: Alert on any failed SAPconnect operations within the last hour

BACKDAYS = 0

STATUS = ERROR

STATE_COLOR = R

COUNTER > 0

DURATION = 1

DURATION_UNIT = H

Use Case 2: Monitor stuck send requests waiting for processing longer than 4 hours

BACKDAYS = 1

STAT_WAIT = X

DURATION > 4

DURATION_UNIT = H

COUNTER > 5

Use Case 3: Monitor email processing failures from specific sender in last 30 minutes

BACKDAYS = 0

SNDART = EMAIL*

STAT_ERROR = X

USER_FLD = SENDER

DURATION = 30

DURATION_UNIT = M

Use Case 4: Monitor long-running communication processing over 1 hour with high request count

BACKDAYS = 0

STATUS = TRANSIT

STATE_COLOR = Y

DURATION > 1

DURATION_UNIT = H

COUNTER > 3

Use Case 5: Monitor multiple status types with UTC time management in global environment

BACKDAYS = 2

STAT_ERROR = X

STAT_WAIT = X

DURATION = 2

DURATION_UNIT = H

MANAGE_IN_UTC = X

USER_FLD = RC_NAME


## EI Function Structure

This table lists all output fields returned by the EI.


| Structure Name | Field Name | Description | Data Type | Component Type |
| --- | --- | --- | --- | --- |
| /SKN/S_SW_01_02_SOST | ADDRESS | Name in address management | CHAR(35) | SO_ADRNAM |
| /SKN/S_SW_01_02_SOST | ADRNR | Addresses: Address Number | CHAR(10) | SO_ADR_NR |
| /SKN/S_SW_01_02_SOST | ATTLEN | Total size of all attachments to a document | CHAR(12) | SO_ATT_LEN |
| /SKN/S_SW_01_02_SOST | COLOR | Not More Closely Defined Area, Possibly Used for Patchlevels | CHAR(4) | CHAR4 |
| /SKN/S_SW_01_02_SOST | COUNTER | Counter for status entries for this send process | CHAR(5) | SO_STA_CNT |
| /SKN/S_SW_01_02_SOST | DEF_STAT | Boolean | CHAR(1) | OS_BOOLEAN |
| /SKN/S_SW_01_02_SOST | DURATION | SW: Duration In Time Units (defined separatly) | INT4(10) | /SKN/E_SW_DURATION |
| /SKN/S_SW_01_02_SOST | DURATION_UNIT | SW: Duration Unit | CHAR(1) | /SKN/E_SW_DURATION_UNIT |
| /SKN/S_SW_01_02_SOST | FORNO | Forwarder: number from the ID | CHAR(12) | SO_FOR_NO |
| /SKN/S_SW_01_02_SOST | FORTP | Forwarder: Object type from ID | CHAR(3) | SO_FOR_TP |
| /SKN/S_SW_01_02_SOST | FORYR | Forwarder: year from the ID | CHAR(2) | SO_FOR_YR |
| /SKN/S_SW_01_02_SOST | ICON | Carrier field for icons | CHAR(132) | ICON_TEXT |
| /SKN/S_SW_01_02_SOST | LENGTH | Size of Document Content | CHAR(12) | SO_OBJ_LEN |
| /SKN/S_SW_01_02_SOST | MARK | Boolean | CHAR(1) | OS_BOOLEAN |
| /SKN/S_SW_01_02_SOST | MESSAGE | Message Text | CHAR(220) | BAPI_MSG |
| /SKN/S_SW_01_02_SOST | MSGID | Message Class | CHAR(20) | SYMSGID |
| /SKN/S_SW_01_02_SOST | MSGNO | Message Number | NUMC(3) | SYMSGNO |
| /SKN/S_SW_01_02_SOST | MSGTP | Type of send request (message or status) | CHAR(1) | SO_MSGTP |
| /SKN/S_SW_01_02_SOST | MSGTXT | Text field length 255: texts | CHAR(255) | SO_TEXT255 |
| /SKN/S_SW_01_02_SOST | MSGTY | Message Type | CHAR(1) | SYMSGTY |
| /SKN/S_SW_01_02_SOST | MSGV1 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_01_02_SOST | MSGV2 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_01_02_SOST | MSGV3 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_01_02_SOST | MSGV4 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_01_02_SOST | NODE | Name of SAPconnect Node or Telephony Server | CHAR(6) | SX_NODE_ID |
| /SKN/S_SW_01_02_SOST | OBJLEN | Size of Document Content | CHAR(12) | SO_OBJ_LEN |
| /SKN/S_SW_01_02_SOST | OBJNO | Object: Number from ID | CHAR(12) | SO_OBJ_NO |
| /SKN/S_SW_01_02_SOST | OBJSNS | Object: Sensitivity (private, functional, ...) | CHAR(1) | SO_OBJ_SNS |
| /SKN/S_SW_01_02_SOST | OBJTP | Code for document class | CHAR(3) | SO_OBJ_TP |
| /SKN/S_SW_01_02_SOST | OBJYR | Object: Year from ID | CHAR(2) | SO_OBJ_YR |
| /SKN/S_SW_01_02_SOST | RC_NAME | Full Name of Person | CHAR(80) | AD_NAMTEXT |
| /SKN/S_SW_01_02_SOST | RECNO | Recipient number from ID | CHAR(12) | SO_REC_NO |
| /SKN/S_SW_01_02_SOST | RECTP | Recipient type from ID | CHAR(3) | SO_REC_TP |
| /SKN/S_SW_01_02_SOST | RECYR | Recipient year from the ID | CHAR(2) | SO_REC_YR |
| /SKN/S_SW_01_02_SOST | SCOMNO | SAPcomm: number of the ID | CHAR(12) | SO_SCOM_NO |
| /SKN/S_SW_01_02_SOST | SCOMTP | SAPcomm ID: ID type | CHAR(3) | SO_SCOM_TP |
| /SKN/S_SW_01_02_SOST | SCOMYR | SAPcomm: year of ID | CHAR(2) | SO_SCOM_YR |
| /SKN/S_SW_01_02_SOST | SENDER | Name in address management | CHAR(35) | SO_ADRNAM |
| /SKN/S_SW_01_02_SOST | SNDART | SAPconnect: Address type | CHAR(3) | SX_ADDRTYP |
| /SKN/S_SW_01_02_SOST | SNDART_DESC | Explanatory short text | CHAR(60) | DDTEXT |
| /SKN/S_SW_01_02_SOST | SNDART_L | Text field length 50 | CHAR(50) | SO_TEXT050 |
| /SKN/S_SW_01_02_SOST | SNDNO | Sender number from the ID | CHAR(12) | SO_SND_NO |
| /SKN/S_SW_01_02_SOST | SNDREQ | Globally Unique Identifier | RAW(16) | OS_GUID |
| /SKN/S_SW_01_02_SOST | SNDTP | Sender type from the ID | CHAR(3) | SO_SND_TP |
| /SKN/S_SW_01_02_SOST | SNDYR | Sender year from the ID | CHAR(2) | SO_SND_YR |
| /SKN/S_SW_01_02_SOST | SOST_MSGID | Message Class | CHAR(20) | SYMSGID |
| /SKN/S_SW_01_02_SOST | SOST_MSGV1 | Message Variable | CHAR(50) | SYMSGV |
| /SKN/S_SW_01_02_SOST | STATE_COLOR | SW: State Color | CHAR(1) | /SKN/E_SW_STATE_COLOR |
| /SKN/S_SW_01_02_SOST | STATE_ICON | SW: State Icon | CHAR(4) | /SKN/E_SW_STATE_ICON |
| /SKN/S_SW_01_02_SOST | STATUS | SAPcomm: status of the sent object | CHAR(12) | SO_REC_STA |
| /SKN/S_SW_01_02_SOST | STATUS_DESC | SW: Message | CHAR(255) | /SKN/E_SW_TEXT |
| /SKN/S_SW_01_02_SOST | STAT_DATE | Date of status | DATS(8) | SO_STADATE |
| /SKN/S_SW_01_02_SOST | STAT_TIME | Time of status | TIMS(6) | SO_STATIME |
| /SKN/S_SW_01_02_SOST | TITEL | Short description of contents | CHAR(50) | SO_OBJ_DES |
| /SKN/S_SW_01_02_SOST | USERNAM | Sender: Name | CHAR(12) | SO_SND_NAM |
| /SKN/S_SW_01_02_SOST | WAIT_DATE | No processing before this date | DATS(8) | SO_WAITDAT |
| /SKN/S_SW_01_02_SOST | WAIT_TIME | No processing before this time | TIMS(6) | SO_WAITTIM |


## ABAP Code


```abap
FUNCTION /SKN/F_SW_01_02_SOST.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(IS_ALERT) TYPE  CHAR1
*"  TABLES
*"      T_SELECT STRUCTURE  RSSELECT OPTIONAL
*"      T_DATA STRUCTURE  /SKN/S_SW_01_02_SOST OPTIONAL
*"----------------------------------------------------------------------
DATA_MULTY: STATE_COLOR /SKN/E_SW_STATE_COLOR,
            DATUM     SY-DATUM,
            DURATION   /SKN/E_SW_DURATION,
            SENDER    XUBNAME,
            SND_DATE SO_DAT_SD,
            SND_TIME SO_TIM_SD,
            SNDART   SX_ADDRTYP.
DATA_SINGLE: BACKDAYS       INT4,
             LANGU          LANGU,
             DURATION_UNIT  /SKN/E_SW_DURATION_UNIT,
             NUMBER         SY-TABIX,
             SUBRC          SY-SUBRC,
             MANAGE_IN_UTC  CHAR1,
             MAX_RECORDS    INT4.
DATA_SINGLE: STAT_WAIT      SOST_STATUS_WAIT,
             STAT_TRANSIT   SOST_STATUS_TRANSIT,
             STAT_OK        SOST_STATUS_OK,
             STAT_ERROR     SOST_STATUS_ERROR,
             STAT_INCONS    SOST_STATUS_INCONS,
             STAT_FUTURE    SOST_STATUS_FUTURE,
             STAT_RETRY     SOST_STATUS_RETRY,
             STAT_DIRECT    SOST_STATUS_DIRECT,
             STAT_ACTIVE    SOST_STATUS_ACTIVE.
DATA : SY_TABIX LIKE SY-TABIX .
DATA : SY_DATLO LIKE SY-DATLO ,
       SY_TIMLO LIKE SY-TIMLO .
DATA:  DATE_FROM LIKE SY-DATUM,
       DATE_TO LIKE SY-DATUM.
DATA : TIME_DIFF TYPE  INT4 .
*data : is_out(1) type C.
*data: PROCESS_ICON TYPE  /SKN/E_SW_STATE_ICON.
*data: R_SND_DATE type SXDATRNGT,
*      RS_SND_DATE like LINE OF R_SND_DATE.
*
*data: R_SND_TIME type SXTIMRNGT,
*      RS_SND_TIME like LINE OF R_SND_TIME.
DATA: LS_STATUS TYPE SOSTSTATUS.
*data: R_SENDER type SXSENDERRNGT,
*      RS_SENDER like LINE OF R_SENDER.
DATA: LV_MAXSEL  TYPE TBMAXSEL.
DATA: LT_SNDRECS TYPE SOXSP2TAB,
      LS_SNDRECS LIKE LINE OF LT_SNDRECS.
DATA: LS_DATA LIKE LINE OF T_DATA.
DATA: LV_DOMNAME TYPE  DDOBJNAME,
      LV_DOMVALUE LIKE  DD07V-DOMVALUE_L,
      LS_DD07V TYPE  DD07V.
DATA: LV_MSG_NUMBER LIKE  BAPIRET2-NUMBER.
DEFINE SET_STATUS.
  LS_STATUS-&1 = LV_STAT_&1.
END-OF-DEFINITION.
*-- Fill Selection Option Tables
   SELECT_MULTY: STATE_COLOR,
                 DATUM,
                 DURATION,
                 SENDER,
                 SND_DATE,
                 SND_TIME,
                 SNDART.
   LV_LANGU = SY-LANGU.
   LV_DURATION_UNIT = 'M'.
   SELECT_SINGLE: BACKDAYS,
                  DURATION_UNIT,
                  MANAGE_IN_UTC,
                  LANGU,
                  MAX_RECORDS.
   SELECT_SINGLE: STAT_WAIT,
                  STAT_TRANSIT,
                  STAT_OK,
                  STAT_ERROR,
                  STAT_INCONS,
                  STAT_FUTURE,
                  STAT_RETRY,
                  STAT_DIRECT,
                  STAT_ACTIVE.
 DATA_SINGLE:   SW_DEST RFCDEST.
 SELECT_SINGLE: SW_DEST.
 DATA: LV_FM TYPE FUNCNAME.
 DATA_MULTY: STATUS SO_REC_STA.
 SELECT_MULTY: STATUS.
*** SET_SY_TIME lv_MANAGE_IN_UTC sy_datlo sy_timlo .
*** TIME_SHIFT sy_datlo sy_timlo . " TIME_SHIFT parameter
""_set_sys_date_time lv_sw_dest sy_datlo sy_timlo.
_GET_CURRENT_DATE_TIME LV_MANAGE_IN_UTC LV_SW_DEST SY_DATLO SY_TIMLO.
*--- Retrieve data
  CLEAR IS_ALERT .
  REFRESH T_DATA .
   IF LV_BACKDAYS IS INITIAL.
     LV_BACKDAYS = 1.
   ENDIF.
   IF R_DATUM[] IS INITIAL .
      RS_DATUM-SIGN = 'I' .
      RS_DATUM-OPTION = 'GE' .
***      DATE_FROM = sy-datum - lv_BACKDAYS .
      DATE_FROM = SY_DATLO - LV_BACKDAYS .
      RS_DATUM-LOW = DATE_FROM .
      APPEND RS_DATUM TO R_DATUM.
   ENDIF.
  IF R_SND_DATE[] IS INITIAL.
    R_SND_DATE[] = R_DATUM[].
   ENDIF.
*  refresh R_SND_DATE.
*  loop at R_DATUM into RS_DATUM.
*    move-corresponding RS_DATUM to RS_SND_DATE.
*    append RS_SND_DATE to R_SND_DATE.
*  endloop.
  "-- Set Status -----
   "-- Set Default Status
   SET_STATUS: WAIT,
               TRANSIT,
               OK,
               ERROR,
               INCONS,
               FUTURE,
               RETRY,
               DIRECT,
               ACTIVE.
   IF  LS_STATUS IS INITIAL.
     LS_STATUS-WAIT = 'X'.
     LS_STATUS-ERROR = 'X'.
     LS_STATUS-INCONS = 'X'.
     LS_STATUS-TRANSIT = 'X'.
     LS_STATUS-OK = 'X'.
   ENDIF.
  "-- Max Records -----
  LV_MAXSEL = 500.
  IF LV_MAX_RECORDS IS NOT INITIAL.
    LV_MAXSEL = LV_MAX_RECORDS.
  ENDIF.
   CALL FUNCTION 'SX_SNDREC_SELECT'
     DESTINATION LV_SW_DEST
     EXPORTING
*      SND_ART             =
       SND_DATE            = R_SND_DATE
       SND_TIME            = R_SND_TIME
*      DEL_DATE            =
*      DEL_TIME            =
       STATUS              = LS_STATUS
*      NOTIFICATIONS       =
       SENDER              = R_SENDER
       MAXSEL              = LV_MAXSEL
*      ALL_WAITING         = 'X'
     IMPORTING
       SNDRECS             = LT_SNDRECS.
   DELETE LT_SNDRECS WHERE: SNDART NOT IN R_SNDART.
   DELETE LT_SNDRECS WHERE: STAT_DATE NOT IN R_SND_DATE.
   DELETE LT_SNDRECS WHERE: STATUS NOT IN R_STATUS.
   LOOP AT LT_SNDRECS INTO LS_SNDRECS.
     MOVE-CORRESPONDING LS_SNDRECS TO LS_DATA.
     IF LV_SW_DEST IS NOT INITIAL.
       LV_FM = '/SKN/FC_SW_01_GET_ADDR_DETAILS'.
     ELSE.
       LV_FM = '/SKN/F_SW_01_GET_ADDR_DETAILES'.
     ENDIF.
     CALL FUNCTION LV_FM  " '/SKN/F_SW_01_GET_ADDR_DETAILES'
       EXPORTING
         ADRNR            = LS_DATA-ADRNR
       IMPORTING
         NAME_TEXT        = LS_DATA-RC_NAME
       EXCEPTIONS
         NO_DATA          = 1
         OTHERS           = 2.
     IF SY-SUBRC <> 0.
     ENDIF.
     IF LS_DATA-MSGTY IS INITIAL.
       IF LS_DATA-STATUS IS NOT INITIAL.
         LS_DATA-MSGTY = LS_DATA-STATUS+0(1).
         T_DATA-MSGV1 = '672'.
       ENDIF.
     ENDIF.
     CALL FUNCTION '/SKN/F_SW_01_02_SOST_STATUS'
       EXPORTING
         MSGTY             = LS_DATA-MSGTY
       IMPORTING
         STATUS_DESC       = LS_DATA-STATUS_DESC
         STATE_COLOR       = LS_DATA-STATE_COLOR
         STATE_ICON        = LS_DATA-STATE_ICON.
     LV_DOMNAME = 'SX_ADDRTYP'.
     LV_DOMVALUE = LS_DATA-SNDART.
     CALL FUNCTION 'DDUT_DOMVALUE_TEXT_GET'
          EXPORTING   NAME          = LV_DOMNAME
                      VALUE         = LV_DOMVALUE
                      LANGU         = LV_LANGU
                      TEXTS_ONLY    = 'X'
          IMPORTING   DD07V_WA      = LS_DD07V
          EXCEPTIONS  OTHERS        = 1.
      IF SY-SUBRC IS INITIAL.
        LS_DATA-SNDART_DESC = LS_DD07V-DDTEXT.
      ENDIF.
     APPEND LS_DATA TO T_DATA.
   ENDLOOP.
  DELETE T_DATA WHERE STATE_COLOR NOT IN R_STATE_COLOR.
*-- Fill Duration Value
 LOOP AT T_DATA .
   SY_TABIX = SY-TABIX.
   T_DATA-DURATION_UNIT = LV_DURATION_UNIT.
    CALL FUNCTION '/SKN/F_SW_GET_TIME_DIFF'
        EXPORTING
          D_FROM            = T_DATA-STAT_DATE
          T_FROM            = T_DATA-STAT_TIME
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
*Add Message text detail
  LOOP AT T_DATA.
    SY_TABIX = SY-TABIX .
    IF NOT T_DATA-MSGID IS INITIAL.
      LV_MSG_NUMBER = T_DATA-STATUS.
      IF LV_MSG_NUMBER IS INITIAL.
        CONTINUE.
      ENDIF.
      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
        DESTINATION LV_SW_DEST
        EXPORTING
          ID                = T_DATA-MSGID        "SOST_MSGID   "STAMID
          NUMBER            = LV_MSG_NUMBER
          LANGUAGE          = LV_LANGU
          TEXTFORMAT        = 'ASC'
*         LINKPATTERN       =
          MESSAGE_V1        = T_DATA-MSGV1        "STAPA1
          MESSAGE_V2        = T_DATA-MSGV2        "STAPA2
          MESSAGE_V3        = T_DATA-MSGV3         "STAPA3
          MESSAGE_V4        = T_DATA-MSGV4         "STAPA4
        IMPORTING
          MESSAGE           = T_DATA-MESSAGE
*         RETURN            =
*       TABLES
*         TEXT              =
                .
      MODIFY T_DATA INDEX SY_TABIX.
    ENDIF.
  ENDLOOP.
*--- Check Alert Information
 READ TABLE T_DATA INDEX 1.
 CHECK NOT SY-TFILL  IS INITIAL .
 IS_ALERT = 'X' .
ENDFUNCTION.
```
