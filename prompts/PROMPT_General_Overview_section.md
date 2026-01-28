# Copy-paste prompt: General Overview section for any EI / function module

Use this in ChatGPT, Claude (web or API), or any LLM. Provide the three input files (or their contents) under the headings at the end, then send the whole thing.

---

```
You are documenting an SAP Exception Indicator (EI) / function module. You will receive the following 3 files as input: (1) an ABAP source code excerpt, (2) a summary of the output structure (fields), (3) a list of parameters (spreadsheet Parameters in the designated file).

Your task: Analyze the function to understand what it does (data sources, aggregation, filters, business domain). Then write ONLY the "General Overview" section for the EI document. Do not output a title, other sections, or preamble.

OUTPUT FORMAT (follow exactly):

1. Intro paragraph (1–2 sentences): What this EI monitors and its business purpose. Wording must be inferred from the code and parameters (e.g. "aggregated sales values", "credit memo volumes by payer", "open items by company code"). Do not assume anything unless the code supports it.

2. "This EI serves as…" paragraph: Start with "This EI serves as an essential control for [inferred domain] by:" then 4–5 bullets describing concrete capabilities (aggregation by period/dimension, thresholds, currencies, filtering, partner/org dimensions etc). Base each bullet on the code (e.g. TOT_CNT, TOT_NETWR, AGGR_PERIOD, partner fields etc).

3. Closing paragraph (2–3 sentences): Value and use cases (e.g. month-end close, performance reviews, exception management, dispute/refund visibility). Infer from the function's logic and parameters.

4. Final sentence: One technical sentence on data sources and processing — which SAP tables or data the function uses (infer from code, e.g. VBAK, VBPA, KNA1) and how it processes them (e.g. "aggregates by the selected time period and dimensions, calculates total counts and values per bucket, then filters by user-specified threshold ranges"). Use table names only if visible in the code or standard for that logic.

Rules:
- Infer domain and wording from the code, structure, and parameters. Do not copy a fixed template; the text must match this specific function.
- Match the structure and depth of the benchmark: same number of paragraphs, same "This EI serves as…" + 4–5 bullets, same type of closing and technical sentence.
- Tone: professional, concise. No implementation details (no internal function names, line numbers, custom namespaces) in the narrative. Standard SAP tables, key fields, and transaction codes are allowed.
- In the narrative, mention only SAP standard entities (e.g. tables, fields, transaction codes). Do not mention custom objects (Z*, /SKN/*, etc.) unless essential to explain the EI's functionality.
- Output ONLY the section: heading "## General Overview" then the four parts above. No document title, no other sections, no preamble.
- **Optional Hint:** After the input file locations you may provide a short phrase under "Hint:". If Hint is empty or omitted, analyze and write as above. If Hint is provided, use it to guide code analysis and align the explanation with that hint (e.g. emphasize purchase-side view, vendor dimension, or a specific use case); the text must still be grounded in the code and parameters.

Benchmark example (format and approach only — do not copy; infer content for the function you are given):

## General Overview

This Exception Indicator (EI) monitors aggregated sales order values in Sales and Distribution (SD) to identify exceptional patterns in sales document values across configurable time periods and organizational dimensions. It provides consolidated visibility into sales volume trends, enabling detection of unusual value concentrations, high-value transactions, and sales pattern anomalies that require management attention.

This EI serves as an essential control for sales management and financial oversight by:
- Aggregating sales order net values by customizable time periods (Month/Week/Quarter/Year) and organizational dimensions
- Identifying exceptional sales volumes that exceed predefined thresholds for transaction counts or value totals
- Monitoring sales patterns across multiple currencies (document currency and foreign currency)
- Providing flexible filtering by sales organization, distribution channel, division, customer, and document characteristics
- Supporting multi-partner analysis through three configurable business partner fields for comprehensive sales relationship visibility

This aggregated monitoring enables organizations to detect high-value transaction clusters, unusual sales concentration patterns, potential revenue recognition issues, and sales trends requiring executive visibility. The EI is particularly valuable for month-end close processes, sales performance reviews, and financial exception management.

The EI retrieves detailed sales order data from SAP SD tables (VBAK - Sales Document Header, VBPA - Sales Document Partner, KNA1 - Customer Master), then aggregates the results by the selected time period and organizational dimensions. It calculates total counts (TOT_CNT), total document currency values (TOT_NETWR), and total foreign currency values (TOT_NETWR_FR) for each aggregation bucket, then filters based on user-specified threshold ranges.

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
