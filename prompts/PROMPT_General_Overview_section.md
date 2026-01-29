# Copy-paste prompt: General Overview section for any EI / function module

Use this in ChatGPT, Claude (web or API), or any LLM. Provide the three input files (or their contents) under the headings at the end, then send the whole thing.

---

```
You are documenting an SAP Exception Indicator (EI) / function module. You will receive the following 3 files as input: (1) an ABAP source code excerpt, (2) a summary of the output structure (fields), (3) a list of parameters (spreadsheet Parameters in the designated file).

Your task: Analyze the function to understand what it does (data sources, aggregation, filters, business domain). Then write ONLY the "General Overview" section for the EI document. Do not output a title, other sections, or preamble.

OUTPUT FORMAT (follow exactly):

1. Intro paragraph (1–2 sentences): What this EI monitors and its business purpose. Wording must be inferred from the code and parameters (e.g. "aggregated sales values", "credit memo volumes by payer", "open items by company code"). Do not assume anything unless the code supports it.

2. "This EI serves as…" paragraph: Start with "This EI serves as an essential control for [inferred domain] by:" then 4–5 bullets describing the **business value** this EI provides (what it enables management to do, detect, or decide). Each bullet must state an **outcome or control**, not a filtering or selection capability. Do **not** list what users can filter by (e.g. do not write "Filtering by date or customer" or "Supporting filtering by status"). Do write what the EI enables (e.g. "Enabling detection of blocked or aged deliveries that require release or escalation", "Supporting identification of credit blocks that delay revenue recognition", "Providing visibility into status duration for prioritization and root-cause analysis"). Infer from the code and parameters but phrase as business value. Do not name parameters or technical fields (e.g. do not mention BACKDAYS, FORWDAYS, DURATION, DURATION_UNIT, DATE_REF_FLD, T_DATA).

3. Closing paragraph (2–3 sentences): Value and use cases (e.g. month-end close, performance reviews, exception management, dispute/refund visibility). Infer from the function's logic and parameters.

4. Final sentence (optional, keep minimal): One short sentence on data sources only if needed for understanding. Mention only the most important SAP tables the function uses; omit if the intro already makes the scope clear. Do not describe joins, processing steps, output structures (e.g. T_DATA), or parameter names.

Rules:
- This section is business-oriented; keep technical details minimal.
- Bullets under "This EI serves as…" describe **business value or outcomes** (what the EI enables management to do, detect, or decide—e.g. detection of exceptions, prioritization, audit support, release of blocks). They do **not** describe filtering or selection capabilities (e.g. do not write "Filtering by sales organization" or "Supporting filtering by partner"; instead write the resulting value, e.g. "Enabling delivery performance and concentration analysis by organization" or "Supporting accountability by partner role").
- Never include in the General Overview: BACKDAYS, FORWDAYS, DURATION (or DURATION_UNIT, etc.), DATE_REF_FLD, T_DATA, or similar parameter/structure names.
- Infer domain and wording from the code, structure, and parameters. Do not copy a fixed template; the text must match this specific function.
- Match the structure and depth of the benchmark: same number of paragraphs, same "This EI serves as…" + 4–5 bullets, same type of closing; final sentence minimal or omitted.
- Tone: professional, concise. No implementation details (no internal function names, line numbers, custom namespaces, joins, output structure names). Mention only the most important tables and central fields, and only when understanding would otherwise be lacking.
- In the narrative, mention only SAP standard entities (e.g. tables, fields, transaction codes) when necessary. Do not mention custom objects (Z*, /SKN/*, etc.) unless essential to explain the EI's functionality.
- Output ONLY the section: heading "## General Overview" then the four parts above. No document title, no other sections, no preamble.
- **Optional Hint:** After the input file locations you may provide a short phrase under "Hint:". If Hint is empty or omitted, analyze and write as above. If Hint is provided, use it to guide code analysis and align the explanation with that hint (e.g. emphasize purchase-side view, vendor dimension, or a specific use case); the text must still be grounded in the code and parameters.

Benchmark example (format and approach only — do not copy; infer content for the function you are given):

## General Overview

This Exception Indicator (EI) monitors aggregated sales order values in Sales and Distribution (SD) to identify exceptional patterns in sales document values across configurable time periods and organizational dimensions. It provides consolidated visibility into sales volume trends, enabling detection of unusual value concentrations, high-value transactions, and sales pattern anomalies that require management attention.

This EI serves as an essential control for sales management and financial oversight by:
- Enabling detection of exceptional sales volumes and value concentrations that exceed predefined thresholds and require management attention
- Supporting identification of high-value transactions and unusual sales patterns for revenue recognition and audit review
- Providing visibility into sales trends by time period and organizational dimension for performance and resource allocation decisions
- Enabling analysis of sales concentration and customer patterns for strategic pricing and engagement
- Supporting accountability and relationship visibility by sold-to, ship-to, and other partner roles for dispute and fulfillment oversight

This aggregated monitoring enables organizations to detect high-value transaction clusters, unusual sales concentration patterns, potential revenue recognition issues, and sales trends requiring executive visibility. The EI is particularly valuable for month-end close processes, sales performance reviews, and financial exception management.

The EI uses sales order data from SAP SD (VBAK, VBPA, KNA1) and aggregates by time period and organizational dimensions.

Output structure / fields file location:

Parameters (Name (Description)) file location:

ABAP code file location:

Hint:
---

Respond with ONLY the "## General Overview" section (heading + four parts). No other text.
```

---

## How to use

1. Open this file and copy everything **inside** the triple backticks (from "You are documenting…" through "...No other text.").
2. Under the headings at the end, provide the **output structure/fields**, **parameters**, and **ABAP code** — either paste the file contents or give file paths/locations (depending on how your chat handles inputs). Optionally, add a short phrase under **Hint:** to steer the explanation (e.g. "purchase-side, vendor dimension"); leave Hint empty or "---" for default behavior.
3. Paste the whole block into ChatGPT, Claude (web), or any LLM and send.
4. Use the model's reply as the General Overview section in your EI document.
