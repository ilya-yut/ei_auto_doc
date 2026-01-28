#!/usr/bin/env python3
"""
Generate EI narrative sections (General Overview, Problem Description, Suggested Resolution)
via LLM. Supports multiple backends; keys from environment only. No keys in source.

Backends (checked in order by generate_ei_doc):
- OpenAI: OPENAI_API_KEY; optional EI_DOC_LLM_BASE_URL for custom endpoint (OpenAI-compatible).
- Anthropic: ANTHROPIC_API_KEY.
- Custom: EI_DOC_LLM_API_KEY + EI_DOC_LLM_BASE_URL (OpenAI-compatible endpoint).
"""
from __future__ import annotations

import re
from typing import Any

# Short benchmark excerpts for style; model should mirror structure and tone, not copy content.
STYLE_OVERVIEW_EXCERPT = """This Exception Indicator (EI) monitors aggregated sales order values in Sales and Distribution (SD) to identify exceptional patterns in sales document values across configurable time periods and organizational dimensions. It provides consolidated visibility into sales volume trends, enabling detection of unusual value concentrations, high-value transactions, and sales pattern anomalies that require management attention.

This EI serves as an essential control for sales management and financial oversight by:
- Aggregating sales order net values by customizable time periods (Month/Week/Quarter/Year) and organizational dimensions
- Identifying exceptional sales volumes that exceed predefined thresholds for transaction counts or value totals
- Monitoring sales patterns across multiple currencies (document currency and foreign currency)"""

STYLE_RESOLUTION_EXCERPT = """**Corrective Actions**
- If unauthorized or erroneous orders are identified, initiate sales document correction procedures using VA02 (Change Sales Order)
- For legitimate high-value orders requiring special approval, escalate to sales management and finance for validation
- Update customer master data (VD02 - Change Customer Sales Data) if partner function issues or credit limit violations are detected
- Configure Dynamic Recipient List (USER_FLD parameter) to automatically route alerts to appropriate sales managers and finance stakeholders based on organizational responsibility"""


def _build_prompt(context: dict) -> str:
    full_name = context.get("full_name", "")
    technical_code = context.get("technical_code", "")
    code_text = context.get("code_text", "")
    structure_summary = context.get("structure_summary", "")
    parameters_summary = context.get("parameters_summary", [])

    params_blob = "\n".join(parameters_summary) if isinstance(parameters_summary, (list, tuple)) else str(parameters_summary)
    code_preview = code_text[:20000] if len(code_text) > 20000 else code_text

    return f"""You are writing documentation for an SAP Exception Indicator (EI). Generate three markdown sections in the exact style and structure of the examples below. Use only the provided context (function name, code, output structure, parameters). Do not copy the example sentences; write new content that fits THIS EI.

## Style examples (match this structure and depth)

**General Overview – example style:**
{STYLE_OVERVIEW_EXCERPT}

**Suggested Resolution – example style (Corrective Actions):**
{STYLE_RESOLUTION_EXCERPT}

## Your task

Output valid markdown with exactly these three level-2 headings. Do not add a document title.

### Required format

1) **## General Overview**
   - 3–4 paragraphs: what this EI monitors, business purpose, then "This EI serves as..." with a bullet list (4–5 bullets), then a closing paragraph on value/use cases.
   - Final sentence: data sources/tables (infer SAP table names from the code if visible, e.g. VBAK, KNA1).

2) **## Problem Description**
   - One short intro sentence (e.g. "Failure to monitor ... creates multiple risks across ...").
   - Then three bold subsections, each with 4–5 bullets:
     - **Financial and Reporting Issues**
     - **Sales Operations and Control Risks**
     - **Management Visibility and Decision-Making Risks**
   - Tailor bullets to the EI's domain (sales, finance, logistics, etc.) using the code and parameters.

3) **## Suggested Resolution**
   - Three bold subsections, each with 4–8 bullets:
     - **Immediate Response**
     - **System Assessment**
     - **Corrective Actions**
   - Where relevant, mention transaction codes (e.g. VA03, VA02, VD02, VK11) and USER_FLD for routing/alerts.
   - Base suggestions on the parameters and output structure provided.

## Context for this EI

- **Full name:** {full_name}
- **Technical code:** {technical_code}

**Output structure / fields:**
{structure_summary}

**Parameters (Name (Description)):**
{params_blob}

**ABAP code (excerpt):**
```
{code_preview[:12000]}
```

Output only the three sections as markdown, starting with "## General Overview" and ending after "## Suggested Resolution". No preamble."""


def _parse_sections(md: str) -> dict[str, str]:
    """Split markdown by ## General Overview, ## Problem Description, ## Suggested Resolution."""
    out = {"overview": "", "problem": "", "resolution": ""}
    if not md or not isinstance(md, str):
        return out

    # Normalize possible headings
    text = md.strip()
    parts = re.split(
        r"\n##\s+General Overview\s*\n",
        text,
        maxsplit=1,
        flags=re.IGNORECASE,
    )
    if len(parts) < 2:
        return out
    rest = parts[1]

    parts = re.split(
        r"\n##\s+Problem Description\s*\n",
        rest,
        maxsplit=1,
        flags=re.IGNORECASE,
    )
    if len(parts) < 2:
        out["overview"] = rest.strip()
        return out
    out["overview"] = parts[0].strip()
    rest = parts[1]

    parts = re.split(
        r"\n##\s+Suggested Resolution\s*\n",
        rest,
        maxsplit=1,
        flags=re.IGNORECASE,
    )
    if len(parts) < 2:
        out["problem"] = rest.strip()
        return out
    out["problem"] = parts[0].strip()
    out["resolution"] = parts[1].strip()
    return out


def _call_openai(prompt: str, api_key: str, base_url: str | None = None, model: str = "gpt-4o") -> str | None:
    """Call OpenAI or an OpenAI-compatible endpoint. Returns response text or None."""
    try:
        from openai import OpenAI
    except ImportError:
        return None
    kwargs: dict[str, Any] = {"api_key": api_key}
    if base_url:
        kwargs["base_url"] = base_url.rstrip("/")
    try:
        client = OpenAI(**kwargs)
        resp = client.chat.completions.create(
            model=model,
            messages=[{"role": "user", "content": prompt}],
            temperature=0.3,
        )
        return (resp.choices[0].message.content or "").strip() or None
    except Exception:
        return None


def _call_anthropic(prompt: str, api_key: str, model: str = "claude-3-5-sonnet-20241022") -> str | None:
    """Call Anthropic Messages API. Returns response text or None."""
    try:
        from anthropic import Anthropic
    except ImportError:
        return None
    try:
        client = Anthropic(api_key=api_key)
        msg = client.messages.create(
            model=model,
            max_tokens=8192,
            messages=[{"role": "user", "content": prompt}],
        )
        if msg.content and isinstance(msg.content, list) and len(msg.content) > 0:
            block = msg.content[0]
            if hasattr(block, "text"):
                return (block.text or "").strip() or None
        return None
    except Exception:
        return None


def generate_narrative_sections(
    context: dict,
    api_key: str | None = None,
    provider: str = "openai",
    base_url: str | None = None,
) -> dict[str, str] | None:
    """
    Call LLM to generate General Overview, Problem Description, Suggested Resolution.
    context must contain: full_name, technical_code, code_text, structure_summary, parameters_summary.
    provider: "openai" (default) or "anthropic". base_url: optional for OpenAI-compatible endpoints.
    Returns dict with keys overview, problem, resolution, or None on missing key/error.
    """
    if not api_key or not api_key.strip():
        return None
    required = ("full_name", "technical_code", "code_text", "structure_summary", "parameters_summary")
    for k in required:
        if k not in context:
            return None

    prompt = _build_prompt(context)
    raw: str | None = None
    if provider == "anthropic":
        raw = _call_anthropic(prompt, api_key)
    else:
        raw = _call_openai(prompt, api_key, base_url=base_url)

    if not raw:
        return None
    return _parse_sections(raw)
