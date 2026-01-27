## MANDATORY BEHAVIOR REMINDER

### STEP 1: IDENTIFY TASK TYPE

Before acting, classify the task:

| Task Type | Trigger | Rules to Apply |
|-----------|---------|----------------|
| DATA VERIFICATION | "compare", "verify", "test", "check against" | RULE 18 + WAIT FOR INPUT |
| FILE EDITING | creating/modifying code or docs | RULES 1-6 (verify after) |
| RESEARCH | "find", "search", "explore" | DOCUMENT AUTHORITY |
| FRONTEND | CSS, React, UI components | FRONTEND DESIGN RULES |
| GIT OPERATIONS | commit, branch, checkout | PRESERVE WORKING CODE |

---

### STEP 2: APPLY RULES FOR TASK TYPE

#### IF DATA VERIFICATION (HITL Testing)

```
HARD STOP PROTOCOL:
1. State: "Waiting for [specific input needed]"
2. FULL STOP - end your response immediately after that line
3. Do NOT add any more sentences, explanations, or preparations
4. Only proceed AFTER user EXPLICITLY provides the requested input
```

**HITL VERIFICATION & CORRECTION CYCLE:**

Find discrepancy → Investigate root cause → FIX THE CODE → Re-upload → Verify fix → Next alert

```
PHASE A: VERIFICATION (Steps 0-6)
----------------------------------
0. USER RE-UPLOADS: User re-uploads the chosen alert for fresh analysis

1. CAPTURE SCREENSHOT: Take Playwright screenshot (right panel only, no sidebar)
   - All 4 discovery cards + Concentration + Key Findings visible
   - Save to: docs/testing/screenshots/hitl-{id}-{YYYYMMDD}.png

2. READ PRIMARY SOURCE: Read the Summary Excel directly
   - Path: docs/skywind-4c-alerts-output/Applications/{MODULE}/{ALERT_ID}/Summary_*.xlsx
   - State: "Reading from: [full path]"

3. SUM & CHECK: Sum financial column, check currency column for existence
   - Extract: record count, financial sum by currency, concentration

4. COMPARE: Build comparison table (UI Screenshot vs Raw Excel)

5. EXPLAIN VERIFICATION: Show and explain what "verified" means
   - State exact values from both sources
   - Show calculation if currency conversion involved

6. VERDICT: PASS or DISCREPANCY FOUND

PHASE B: CORRECTION (Steps 7-8, if discrepancy found)
-----------------------------------------------------
7. CORRECT THE DISCREPANCY:
   - Investigate root cause (backend code, LLM prompts, conversion logic)
   - Identify exact file and line causing the issue
   - Make the necessary code change
   - Verify the fix with read_file after edit

8. REPEAT: Go back to Step 0 (user re-uploads same alert)

PHASE C: POST-CORRECTION COMPARISON (Steps 9-11)
------------------------------------------------
9. COMPARE BEFORE/AFTER: See exact difference in values
   - Example: Material 7390143 metric_value changed from X to Y

10. VERIFY FIX WORKED: Confirm new values match expected
    - Example: Old ~$22.6M → New ~$18K (matches raw data)

11. DOCUMENT IN EXCEL: Record in HITL-Manual-Sampling-Template.xlsx
    - Include: screenshot path, source file path, old value, new value, fix applied, final verdict

REPEAT UNTIL: All discrepancies resolved → PASS → Move to next alert
```

**RULE 18 - PRIMARY SOURCE DATA INTEGRITY:**

- Use ONLY files explicitly provided by user
- NEVER substitute pre-calculated, cached, or derived files
- State: "Reading from: [exact file path]"
- If user says "compare against X" → READ X, not something else

#### IF FILE EDITING

- NEVER claim success without reading file afterward
- Say "I attempted..." not "I have implemented..."
- Verify EACH file individually

#### IF RESEARCH

- Check llm_handover.md FIRST
- Never use first-found document without verifying authority
- Update existing docs, don't create new ones

#### IF FRONTEND

- No AI-recognizable patterns (colored left bars, gradients)
- Follow THA frozen specs
- Check frontend-design-checklist.md

#### IF GIT

- Check git status before any destructive operation
- Never checkout/reset without confirming uncommitted changes

---

### STEP 3: HONESTY RULES (ALWAYS APPLY)

- No yesman behavior - truth over convenience
- Honest uncertainty > confident incorrectness
- If uncertain, ADMIT IT - don't guess plausibly
- Real limitations > fake capabilities

---

### STEP 4: ANTI-SHORTCUT GATE

Before executing, ask yourself:

```
[ ] Am I taking a shortcut? (using cached/derived data, skipping verification)
[ ] Am I proceeding without required user input?
[ ] Am I generating output just to "be helpful"?
```

If ANY checkbox is YES → STOP. Do not proceed.

---

### STEP 5: PROHIBITED BEHAVIORS (NEVER DO THESE)

```
❌ Do NOT pre-read files before user provides required input
❌ Do NOT outline "what I'll do when you provide X"
❌ Do NOT say "meanwhile", "in preparation", "let me also", "I'll start by"
❌ Do NOT generate ANY text after "Waiting for: [X]"
❌ Do NOT proceed until user EXPLICITLY provides requested input
❌ Do NOT anticipate what user might want next
❌ Do NOT "prepare" anything while waiting
❌ Do NOT add helpful suggestions after stating you're waiting
```

**What counts as "user provided input":**

- User sends the specific file/screenshot/data you requested
- NOT: User says "go ahead" without providing the input
- NOT: You deciding "I'll just start anyway"

---

### RESPONSE FORMAT

**For verification tasks requiring user input:**

```
Ready.
```

[ONE WORD. NOTHING ELSE. STOP.]

**For action tasks where you have everything needed:**

```
Rules acknowledged.
Task type: [TYPE]
Primary source: [exact file/doc I will use]
Proceeding with: [specific action]
```

---

### VIOLATION = WASTED TIME AND MONEY. FOLLOW THE PROTOCOL
