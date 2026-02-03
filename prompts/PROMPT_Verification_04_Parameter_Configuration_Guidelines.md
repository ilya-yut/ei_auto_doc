# Verification prompt: 04_response.md (Parameter Configuration Guidelines)

Use this prompt to **verify** an existing `04_response.md` file, **search for useless phrases in any wording**, and **replace** each parameter’s explanation block where violations are found. The output is an updated `04_response.md` that complies with the section‑4 rules.

---

## Role and task

You are verifying and fixing the **Parameter Configuration Guidelines** section in `04_response.md`. You receive the file (path or pasted content).

**Your task:**

1. **Search** every parameter’s **main explanation** (the paragraph(s) under each `**PARAM** (Description):` before any Options or Connection subsection) for useless or forbidden content — in **any wording**, not only fixed phrases.
2. **Identify** every parameter block whose main explanation contains at least one useless sentence or violates a forbidden principle.
3. **Replace** each such block: rewrite only the **main explanation** so it complies. Keep the parameter header and any Options/Connection subsections unless they too violate; then fix only those parts. Preserve parameter order and structure; do not remove or merge entries.
4. **Output** the **full** corrected "### Parameter Configuration Guidelines" subsection (heading through last parameter block). Do not output only the changed blocks.

---

## Assumption

**Assume the reader already knows that all parameters can be used as filters and as output.** Do not state or imply this in the explanations; it is redundant.

---

## What counts as useless (principle-based)

A sentence in a parameter’s **main explanation** is **useless** if it fails any of these criteria:

### Generic or filler

- It could apply to many other parameters with only the parameter name or concept swapped.  
  **Criterion:** The sentence does not tie the explanation to *this* parameter or *this* function.
- It only states that the parameter can be set, used, or applied to narrow/limit/filter/focus the result, **without** saying what the parameter **is** in this function or **how** this function uses it in business terms.  
  **Criterion:** No function- or parameter-specific information (source of the value, when/where in the process it is used, or business meaning).

**Self-check (every sentence):**  
"If I replaced this parameter name with another, would this sentence still be true?"  
If **yes**, the sentence is generic — treat it as useless and replace the explanation block.

### Redundant: stating that parameters can be used as filters or output

**Exclude** (remove) any part of an explanation that states or implies this idea in **any wording**. The reader already knows it; such wording is redundant.

This includes (in any phrasing) clauses that say the parameter appears in the output, is provided in the output, can be used when exposed, is used for selection or post-processing, or that the EI uses/reads/applies it when reading X data — when that clause adds no other information (e.g. which business object or which step in the process). **Remove** those clauses. Keep only what the parameter **is** in business terms and, when relevant, where the value comes from or how it figures in this function’s logic (e.g. which date is used for the monitoring window, which field is resolved to another).

### Forbidden: filter/output classification

Do **not** describe any parameter in a way that classifies it as "only for output," "not used for filtering," "only for display," or similar — in **any wording**. If the current text does that, replace the block. (See also the assumption above: do not state that a parameter can be used as filter or output — it is redundant.)

### Forbidden: results-limited boilerplate

Do **not** use sentences that only restate that selection or a supplied value narrows the result set. Replace with **what** the parameter is and **how** it is used in this function.

### Other filler

Remove or replace any phrase that adds no function- or parameter-specific information, regardless of exact wording.

---

## What every main explanation must include

After replacement, each main explanation must include **at least one** of:

- Where the value comes from in this function.
- When or where in the process the parameter is used (in business terms).
- What it means in business terms.

Every sentence must add **function- or parameter-specific** information. Remove or replace any sentence that does not.

---

## How to replace an explanation block

1. **Keep** the line: `**PARAM_NAME** (Description):`
2. **Replace** only the main explanation (the text before the next parameter header or before Options/Connection subsections).  
   - Use **business-oriented** language: what the parameter is and when/where in the process it is used.  
   - Do **not** use implementation details (e.g. SELECT/JOIN, table/field names, internal variable or system names) unless essential for clarity.  
   - Do **not** classify the parameter as filter vs non-filter or output-only.
3. **Keep** Options and Connection subsections unchanged unless they violate the same principles; if they do, fix only the violating sentences.

---

## Output format

Output the **entire** corrected Parameter Configuration Guidelines section:

- Start with `### Parameter Configuration Guidelines`, then the IMPORTANT line with the correct parameter count, then every parameter block in the **exact same order** as in the input.
- End with the last parameter block. No document title, preamble, or other sections.

---

## Input

**04_response.md** (path or paste):

[Provide the path to `scripts/pipeline/run/04_response.md` or paste the full content below.]

---

Respond with the **full** corrected content of the "### Parameter Configuration Guidelines" subsection only. No other sections or commentary.
