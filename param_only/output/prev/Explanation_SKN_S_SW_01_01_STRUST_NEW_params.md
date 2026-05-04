# Parameters: SKN_S_SW_01_01_STRUST_NEW

### Parameters Reference Table

This table lists all configurable input parameters. Users set values for these parameters to filter and control the EI's data selection and processing logic.

| # | Parameter | Description | Type | Length | Decimal | Data Element | Domain |
|---|-----------|-------------|------|--------|---------|--------------|--------|
| 1 | APPLIC | SSL Client Identity | CHAR | 6 | 0 | SSFAPPLSSL | SSFAPPLSSL |
| 2 | BACKDAYS | Back Days |  | 0 | 0 |  |  |
| 3 | CONTEXT | Application Context of a PSE | CHAR | 4 | 0 | PSECONTEXT | PSECONT |
| 4 | DESCRIPT | SSF application | CHAR | 50 | 0 | SSFAPPLTXT | SSFAPPLTXT |
| 5 | FORWDAYS | Forward Days |  | 0 | 0 |  |  |
| 6 | ISSUER | Own Issuer | CHAR | 255 | 0 | /SKN/E_SW_ISSUER | /SKN/D_SW_ISSUER |
| 7 | LIST_ISSUER | List Issuer | CHAR | 255 | 0 | /SKN/E_SW_LIST_ISSUER | /SKN/D_SW_ISSUER |
| 8 | LIST_SUBJECT | List Subject | CHAR | 255 | 0 | /SKN/E_SW_LIST_SUBJECT | /SKN/D_SW_SUBJECT |
| 9 | LIST_VALID_FROM | List Valid From | DATS | 8 | 0 | /SKN/E_SW_LIST_VALID_FROM | DATE |
| 10 | LIST_VALID_TO | List Valid To | DATS | 8 | 0 | /SKN/E_SW_LIST_VALID_TO | DATE |
| 11 | SUBJECT | Own Subject | CHAR | 255 | 0 | /SKN/E_SW_SUBJECT | /SKN/D_SW_SUBJECT |
| 12 | VALID_FROM | Own Valid From | DATS | 8 | 0 | /SKN/E_SW_OWN_VALID_FROM | DATE |
| 13 | VALID_TO | Own Valid To | DATS | 8 | 0 | /SKN/E_SW_OWN_VALID_TO | DATE |

### Parameter Configuration Guidelines

**IMPORTANT:** This section provides configuration guidance for ALL 13 parameters listed in the Parameters Reference Table above.

**APPLIC** (SSL Client Identity):

Identifies the SSL client application (SSF application) for which the Personal Security Environment (PSE) and its certificates are evaluated. The EI retrieves PSE information per application and context; APPLIC is passed to the PSE lookup and determines which SSL client identities are included. The result set is filtered by the supplied APPLIC range so only matching applications appear in the output.

**Work together / Connection:** APPLIC and CONTEXT together identify a specific PSE. Both are required to target a given PSE; configure them together when focusing on a particular application and context combination.

**BACKDAYS** (Back Days):

Defines how many days in the past from the current date the validity window extends. The value is used to build the lower bound of the certificate validity window: certificates whose validity end (or list validity end) falls within the window from (current date + BACKDAYS) to (current date + FORWDAYS) are included. When not supplied, a default is applied so that a wide lookback is used. When supplied, the value is interpreted as a positive lookback and stored internally as a negative offset for the range calculation.

**CONTEXT** (Application Context of a PSE):

Application context of the PSE (e.g. standard context for an SSL client). The EI uses CONTEXT together with APPLIC to retrieve PSE and certificate information; each PSE is uniquely identified by the pair (APPLIC, CONTEXT). The result set is filtered by the CONTEXT range so only certificates for the selected contexts are returned.

**Work together / Connection:** CONTEXT and APPLIC together identify a specific PSE. Use both when targeting a particular SSL client and context.

**DESCRIPT** (SSF application):

SSF application description (long text) for the SSL client. It corresponds to the descriptive text of the SSF application and is used to identify or label which application entries are considered. When used as a selection criterion, it helps focus on specific application descriptions in the result set.

**FORWDAYS** (Forward Days):

Defines how many days forward from the current date the validity window extends. Together with BACKDAYS it defines the certificate validity window: only certificates whose validity end date (or list validity end) falls within the range from (current date + BACKDAYS) to (current date + FORWDAYS) are included. When not supplied, a default is applied so that a wide forward window is used.

**Work together / Connection:** FORWDAYS and BACKDAYS define the validity window. Use them together to control how far in the past and future certificate validity is evaluated.

**ISSUER** (Own Issuer):

Issuer of the own (PSE) certificate. It identifies the certificate authority that issued the certificate stored in the PSE. When set, it restricts which certificates are included based on the issuer value; it maps to the issuer attribute of the certificate returned for the PSE.

**LIST_ISSUER** (List Issuer):

Issuer of certificates in the certificate list associated with the PSE. When set, it restricts which list entries or certificates are considered based on the list issuer. It corresponds to the issuer attribute of certificates in the PSE certificate list.

**LIST_SUBJECT** (List Subject):

Subject of certificates in the PSE certificate list. When set, it restricts which list entries are considered based on the subject. It corresponds to the subject attribute of certificates in the list.

**LIST_VALID_FROM** (List Valid From):

Start date of the validity period for certificates in the PSE certificate list. When set, it restricts list entries by the start of their validity period. It is used together with list validity end when evaluating whether a certificate falls within the monitoring window.

**LIST_VALID_TO** (List Valid To):

End date of the validity period for certificates in the PSE certificate list. The EI uses this date (or the own certificate validity-to when list validity-to is empty) to compute the time difference used for the validity window check. When set, it restricts list entries by the end of their validity period.

**SUBJECT** (Own Subject):

Subject of the own (PSE) certificate. It identifies the entity (e.g. system or client) the certificate belongs to. When set, it restricts which certificates are included based on the subject; it maps to the subject attribute of the certificate stored in the PSE.

**VALID_FROM** (Own Valid From):

Start date of the validity period of the own (PSE) certificate. When set, it restricts which certificates are included based on the start of their validity period. It is used as a selection criterion for the certificate validity range.

**VALID_TO** (Own Valid To):

End date of the validity period of the own (PSE) certificate. When set, it restricts which certificates are included based on the end of their validity period. The EI uses certificate validity (own or list) together with FORWDAYS and BACKDAYS to determine whether a certificate falls within the monitoring window.

**Work together / Connection:** VALID_FROM and VALID_TO define the own certificate validity range. FORWDAYS and BACKDAYS define the monitoring window in days from the current date; certificates whose validity end falls in that window are included.


### Parameter Relationships

**PSE identification (APPLIC and CONTEXT)**

- **APPLIC** and **CONTEXT** together uniquely identify a Personal Security Environment (PSE). The EI retrieves PSE and certificate information per (APPLIC, CONTEXT) pair and filters the result set by both parameters.

**Certificate validity window (VALID_FROM, VALID_TO, FORWDAYS, BACKDAYS)**

- **VALID_FROM** and **VALID_TO** define the validity date range used as selection criteria for the own certificate.
- **FORWDAYS** and **BACKDAYS** define the monitoring window in days from the current date: only certificates whose validity end (or list validity end) falls between (current date + BACKDAYS) and (current date + FORWDAYS) are included.
- Together, these parameters control which certificates are selected by validity: VALID_FROM/VALID_TO restrict by certificate validity dates; FORWDAYS/BACKDAYS restrict by how close validity end is to today.

**List certificate validity (LIST_VALID_FROM, LIST_VALID_TO)**

- **LIST_VALID_FROM** and **LIST_VALID_TO** define the validity date range for certificates in the PSE certificate list. They are used to filter or evaluate list entries by their validity period; LIST_VALID_TO is also used when computing the time difference for the validity window check when the own certificate validity-to is not the determining factor.


### Default Values and Practical Configuration Examples

**Default Values**

- **FORWDAYS** — Default: 5000 (when not supplied).
- **BACKDAYS** — Default: -5000 (when not supplied).

**Practical Configuration Examples**

1. **Single PSE, wide validity window**  
   Set **APPLIC** and **CONTEXT** to the SSL client identity and PSE context you want to monitor (e.g. one application and one context). Leave **FORWDAYS** and **BACKDAYS** at default to include certificates whose validity end falls within the default window (current date −5000 to +5000 days).

2. **Multiple applications, narrow validity window**  
   Set **APPLIC** to a range of SSL client identities and **CONTEXT** to the relevant context(s). Set **FORWDAYS** (e.g. 90) and **BACKDAYS** (e.g. 0 or a negative equivalent) so that only certificates expiring in the next 90 days (or within the chosen window) are reported.

3. **Focus on expiring certificates with issuer and subject filters**  
   Set **APPLIC** and **CONTEXT** to target the desired PSE(s), **FORWDAYS** and **BACKDAYS** to define the alert window (e.g. next 30 days), **VALID_FROM** and **VALID_TO** to restrict by own certificate validity range, and **ISSUER** or **SUBJECT** (and optionally **LIST_ISSUER**, **LIST_SUBJECT**, **LIST_VALID_FROM**, **LIST_VALID_TO**) to restrict by certificate attributes and list validity. Use this to find expiring certificates from a specific CA or subject within a given validity range.


### EI Function Structure

This table lists all output fields returned by the EI. These fields contain the results of the EI's data retrieval and calculations.

| Structure Name | Field Name | Description | Data Type | Component Type |
|----------------|------------|-------------|-----------|----------------|
| /SKN/S_SW_01_01_STRUST_NEW | APPLIC | SSL Client Identity | CHAR(6) | SSFAPPLSSL |
| /SKN/S_SW_01_01_STRUST_NEW | CONTEXT | Application Context of a PSE | CHAR(4) | PSECONTEXT |
| /SKN/S_SW_01_01_STRUST_NEW | DESCRIPT | SSF application description (long text) | CHAR(50) | SSFAPPLTXT |
| /SKN/S_SW_01_01_STRUST_NEW | ISSUER | Own Issuer | CHAR(255) | /SKN/E_SW_ISSUER |
| /SKN/S_SW_01_01_STRUST_NEW | LIST_ISSUER | List Issuer | CHAR(255) | /SKN/E_SW_LIST_ISSUER |
| /SKN/S_SW_01_01_STRUST_NEW | LIST_SUBJECT | List Subject | CHAR(255) | /SKN/E_SW_LIST_SUBJECT |
| /SKN/S_SW_01_01_STRUST_NEW | LIST_VALID_FROM | List Valid From | DATS(8) | /SKN/E_SW_LIST_VALID_FROM |
| /SKN/S_SW_01_01_STRUST_NEW | LIST_VALID_TO | List Valid To | DATS(8) | /SKN/E_SW_LIST_VALID_TO |
| /SKN/S_SW_01_01_STRUST_NEW | SUBJECT | Own Subject | CHAR(255) | /SKN/E_SW_SUBJECT |
| /SKN/S_SW_01_01_STRUST_NEW | VALID_FROM | Own Valid From | DATS(8) | /SKN/E_SW_OWN_VALID_FROM |
| /SKN/S_SW_01_01_STRUST_NEW | VALID_TO | Own Valid To | DATS(8) | /SKN/E_SW_OWN_VALID_TO |