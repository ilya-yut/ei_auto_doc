# Copy-paste prompt: Problem Description and Suggested Resolution for any EI / function module

Use this in ChatGPT, Claude (web or API), or any LLM. Provide the three input files (or their contents) under the headings at the end, then send the whole thing.

---

```
You are documenting an SAP Exception Indicator (EI) / function module. You will receive the following 3 files as input: (1) an ABAP source code excerpt, (2) a summary of the output structure (fields), (3) a list of parameters (spreadsheet Parameters in the designated file).

Your task: Analyze the function to understand what it does (data sources, aggregation, filters, business domain). Then write ONLY the "Problem Description" and "Suggested Resolution" sections for the EI document. Do not output a title, other sections, or preamble.

OUTPUT FORMAT (follow exactly):

## Problem Description

1. Intro sentence (one sentence): "Failure to monitor [what this EI monitors, inferred from code] creates multiple risks across [domain-appropriate areas, e.g. financial reporting, operational management, and compliance]." Wording must be inferred from the code and parameters. Do not assume anything unless the code supports it.

2. Exactly three bold subsections, each with 4–5 bullets. Use domain-adapted titles for the three subsections (e.g. **Financial and Reporting Issues**; **Operational/Control Risks** or **Sales Operations and Control Risks** or **Procurement and Control Risks** or **System Stability** or **Government Compliance**; **Management Visibility and Decision-Making Risks**). Choose titles that fit the function's domain. Base each bullet on risks that arise when the EI's monitoring is absent or inadequate, inferred from the function's logic and parameters. Describe risks in **business language**; do not name parameters, fields, or structures (e.g. do not mention BACKDAYS, DURATION, DATE_REF_FLD, T_DATA).

## Suggested Resolution

3. Exactly three bold subsections, each with 4–8 bullets:
   - **Immediate Response** — first-line actions (review flagged data, verify high-value items, check status, clarify business context). Base on what the EI outputs and which transactions the code uses. Describe in **business language**; do not name parameters, fields, or structures.
   - **System Assessment** — analysis steps (e.g. time period and dimension review, comparison to prior periods). Base on the function's logic. Describe in **business language**; do not name parameters, fields, or structures (e.g. no BACKDAYS, DATE_REF_FLD, DURATION, T_DATA).
   - **Corrective Actions** — follow-up actions (correct erroneous data, escalate, update master data, adjust monitoring scope, document, schedule recurring runs). Base on the function's domain. Describe in **business language**; transaction codes (e.g. VA02, VL03N) are allowed when they help the reader act; do not name parameters, fields, or structures.

Rules:
- This section is business-oriented; keep technical details minimal.
- Never include in the narrative: BACKDAYS, FORWDAYS, DURATION (or DURATION_UNIT, etc.), DATE_REF_FLD, T_DATA, or similar parameter/structure names. Mention only the most important tables and central fields when understanding would otherwise be lacking; no joins, no output structure names.
- Infer domain and wording from the code, structure, and parameters. Do not copy a fixed template; the text must match this specific function.
- Match the structure and depth of the benchmark: same number of subsections (exactly three for Problem Description, exactly three for Suggested Resolution), same type of intro sentence and bullets. Adapt subsection titles to the function's domain (e.g. Financial and Reporting; Operational/Control or Procurement/Sales Operations; Management Visibility; System Stability; Government Compliance as appropriate).
- Tone: professional, concise. No implementation details (no internal function names, line numbers, custom namespaces). Bullets must be in business language (what goes wrong, what to do); do not list parameter names, field names, or structure names. Transaction codes (e.g. VA03, VL03N) are allowed when they help the reader act.
- In the narrative, mention only SAP standard entities (e.g. tables, fields, transaction codes) when necessary. Do not mention custom objects (Z*, /SKN/*, etc.) unless essential to explain the EI's functionality.
- Output ONLY the two sections: heading "## Problem Description" then the intro and three subsections; heading "## Suggested Resolution" then the three subsections. No document title, no other sections, no preamble.
- **Optional Hint:** After the input file locations you may provide a short phrase under "Hint:". If Hint is empty or omitted, analyze and write as above. If Hint is provided, use it to guide code analysis and align the explanation with that hint (e.g. emphasize purchase-side view, vendor dimension, or a specific use case); the text must still be grounded in the code and parameters.

Benchmark example (format and approach only — do not copy; infer content for the function you are given). Source: reference files\Explanation_Credit Memo Monthly volume by Payer_$1M in LC_200019_000012__EI__SW_10_01_ORD_VAL_TOT.md

## Problem Description

Failure to monitor aggregated sales order values creates multiple risks across financial reporting, operational management, and compliance:

**Financial and Reporting Issues**
- Undetected high-value order concentrations can distort period-over-period sales trend analysis and forecasting accuracy
- Exceptional sales volumes in specific periods may indicate revenue recognition timing issues or premature booking
- Unusual value patterns in foreign currency transactions can signal currency risk exposure requiring hedging actions
- Aggregated sales anomalies may delay month-end close processes when discovered late during financial review
- Concentrated high-value orders in specific sales organizations or channels can mask underlying performance issues in other business units

**Sales Operations and Control Risks**
- Large transaction clusters without proper visibility may indicate unauthorized discounting or pricing violations
- Exceptional values in specific customer segments could signal credit risk concentration requiring management intervention
- Unusual sales patterns by partner functions (sold-to, ship-to, bill-to) may indicate customer master data quality issues
- High-volume activity in specific divisions or distribution channels may reflect operational bottlenecks or resource constraints
- Atypical aggregated values could indicate data entry errors or system integration failures requiring immediate correction

**Management Visibility and Decision-Making Risks**
- Lack of aggregated value monitoring delays executive awareness of significant business trends and market shifts
- Unidentified sales concentration patterns can lead to missed opportunities for strategic pricing or customer engagement
- Exceptional transaction volumes may require additional audit scrutiny or compliance review but go unnoticed
- Absence of multi-dimensional sales analysis limits ability to optimize sales territory assignments and resource allocation

## Suggested Resolution

**Immediate Response**
- Review the aggregated sales values flagged by the EI to understand the nature and scope of the exceptional pattern (threshold violations, value concentration, period-specific spikes)
- Verify the authenticity of high-value orders using transaction VA03 (Display Sales Order) to confirm legitimacy and proper authorization
- Check sales document status and processing progress to ensure no manual intervention or corrections are pending
- Identify the business context for exceptional volumes: promotional campaigns, large customer orders, seasonal patterns, or data quality issues

**System Assessment**
- Analyze the aggregation dimensions (time period, organizational scope) to understand which factors drive the exceptional pattern
- Review historical trends by comparing current aggregated values to prior periods using the same criteria
- Examine currency-specific patterns (document currency and foreign currency) to identify exposure or booking issues
- Assess partner and customer distribution to identify relationship patterns or master data inconsistencies
- Investigate document type and category to determine if exceptions are type-specific
- Validate the timing basis for aggregation (e.g. requested delivery date, document date, creation date) to ensure the monitoring window is appropriate

**Corrective Actions**
- If unauthorized or erroneous orders are identified, initiate sales document correction procedures using VA02 (Change Sales Order)
- For legitimate high-value orders requiring special approval, escalate to sales management and finance for validation
- Update customer master data (VD02 - Change Customer Sales Data) if partner or credit issues are detected
- Adjust pricing or discounting arrangements using VK11 (Create Condition Record) if pricing violations are confirmed
- Implement additional monitoring controls by tightening threshold criteria for future executions
- Document exceptional patterns and business justifications for audit trail and management reporting purposes
- Establish recurring EI execution schedules to provide continuous visibility into sales value trends and concentration risks
- Configure alert routing to appropriate sales managers and finance stakeholders based on organizational responsibility

Output structure / fields file location:

Parameters (Name (Description)) file location:

ABAP code file location:

Hint:
---

Respond with ONLY the "## Problem Description" and "## Suggested Resolution" sections (headings + content as above). No other text.
```

---

## How to use

1. Open this file and copy everything **inside** the triple backticks (from "You are documenting…" through "...No other text.").
2. Under the headings at the end, provide the **output structure/fields**, **parameters**, and **ABAP code** — either paste the file contents or give file paths/locations (depending on how your chat handles inputs). Optionally, add a short phrase under **Hint:** to steer the explanation (e.g. "purchase-side, vendor dimension"); leave Hint empty or "---" for default behavior.
3. Paste the whole block into ChatGPT, Claude (web), or any LLM and send.
4. Use the model's reply as the Problem Description and Suggested Resolution sections in your EI document.
