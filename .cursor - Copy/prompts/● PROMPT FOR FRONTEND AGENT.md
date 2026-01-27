# PROMPT FOR FRONTEND AGENT (Sally)

**Last Updated:** 2026-01-16 10:35
**Updated By:** Orchestrator (Claude)
**Status:** ACTIVE

---

## COORDINATION PROTOCOL

```
┌─────────────────────────────────────────────────────────────────┐
│  YOU ARE: Sally (Frontend Agent)                                │
│  READ: This file for your tasks                                │
│  READ: ORCHESTRATOR_HUB.md for dependencies & coordination     │
│  WRITE: Status updates to THIS file                            │
│  COORDINATE: Through Orchestrator for cross-agent work         │
└─────────────────────────────────────────────────────────────────┘
```

---

## ⚠️ MANDATORY PRE-TASK READING (Code of Conduct)

**BEFORE starting ANY task, you MUST read these files IN ORDER:**

```
1. .claude/prompts/frontend-pre-task.md     → Frontend-specific rules & FROZEN specs
2. .claude/rules/anti-hallucination-rules.md → Verification behavior (18 rules)
3. .claude/rules/preserve-working-code.md    → Git safety rules
4. .claude/rules/document-authority.md       → Document hierarchy
5. .claude/rules/frontend-design-checklist.md → THA visual standards (detailed)
```

**After reading, acknowledge:**
```
"I have read the frontend pre-task protocol and all mandatory rules files.
I will follow THA design standards, verify all changes, and avoid AI-recognizable patterns."
```

---

## CURRENT STATUS

### ⏸️ BLOCKED: SOKB Processing Flow

**You are WAITING for Backend (Winston) to complete BE-SOKB-001**

The SOKB Classification and Grouping cards are NOT displaying because the backend
is missing `input_data` and `output_data` in the rule logging.

**DO NOT:**
- Mark SOKB as complete
- Test SOKB cards until backend confirms fix
- Move to next tasks until SOKB is verified

**WHEN UNBLOCKED:**
1. Wait for Winston to confirm BE-SOKB-001 is complete
2. Upload a NEW alert for fresh analysis
3. Open Actions Log → Processing Flow tab → READ stage → Results tab
4. Verify SOKB Classification card shows column type counts
5. Verify SOKB Grouping card shows dimension summaries
6. THEN mark FE-SOKB-001 and FE-SOKB-002 as complete

---

## YOUR TASK QUEUE

### Priority 0 (Blocked - Wait)

| Task ID | Description | Status | Blocked By |
|---------|-------------|--------|------------|
| FE-SOKB-001 | Display SOKB Classification cards | ⏸️ BLOCKED | BE-SOKB-001 |
| FE-SOKB-002 | Display SOKB Grouping cards | ⏸️ BLOCKED | BE-SOKB-001 |

### Priority 1 (Next Up - After SOKB)

| Task ID | Description | Status | Depends On |
|---------|-------------|--------|------------|
| FE-RAW-001 | Implement "View Raw" button functionality | 📋 PLANNED | BE-RAW-002 (API) |
| FE-RAW-002 | Add pagination UI (100 rows/page, page numbers) | 📋 PLANNED | FE-RAW-001 |
| FE-ANALYZE-001 | Implement "Analyze" button modal | 📋 PLANNED | BE-ANALYZE-001 |
| FE-PIPE-001 | Implement ProcessingFlowTab component | 📋 PLANNED | BE-PIPE-003 |

### Priority 2 (Later)

| Task ID | Description | Status | Depends On |
|---------|-------------|--------|------------|
| FE-RAW-003 | Add "Go to row" navigation | 📋 PLANNED | FE-RAW-002 |
| FE-ANALYZE-002 | Build pivot dropdown selectors UI | 📋 PLANNED | FE-ANALYZE-001 |
| FE-ANALYZE-003 | Add nested grouping (multi-dimension) | 📋 PLANNED | FE-ANALYZE-002 |
| FE-ANALYZE-004 | Add drill-down from aggregated to raw rows | 📋 PLANNED | FE-ANALYZE-002 |

---

## DESIGN DECISIONS (Already Made)

These decisions were made in brainstorming sessions. **Do not re-decide.**

### View Raw Button
- Pagination: 100 rows per page
- Navigation: Page numbers + "Go to row" input
- Data scope: Discovery rows only (not full alert)
- Export: CSV download (streamed from backend)

### Analyze Button
- UI Complexity: Dropdown selectors (middle ground)
- Grouping: Up to 3 nested levels (e.g., Vendor → Material → Plant)
- Measures: Sum, Count, Avg, Min, Max
- Filters: Currency, custom conditions
- Data scope toggle: "This Discovery" vs "Full Alert"
- Drill-down: Click aggregated row → see raw rows

### Reference Documents
- `Discovery_Sections_Functions_Brainstorm.md` - Full design discussion
- `THA Pipeline Flow Integration Plan.md` - ProcessingFlowTab spec

---

## FILES YOU OWN

```
frontend/src/pages/alert-discoveries/
├── features/
│   ├── analysis-log/
│   │   ├── ProcessingFlowTab.tsx      ← FE-PIPE-001 (create)
│   │   └── ProcessingFlowTab.css      ← FE-PIPE-001 (create)
│   ├── section-actions/
│   │   ├── ViewRawModal.tsx           ← FE-RAW-001 (modify)
│   │   └── AnalyzeModal.tsx           ← FE-ANALYZE-001 (create)
```

---

## MESSAGES FROM ORCHESTRATOR

```
[2026-01-16 10:35] ORCHESTRATOR:
STATUS: You are BLOCKED on SOKB tasks
ACTION: Wait for BE-SOKB-001 completion
NEXT: Prepare mentally for View Raw implementation (FE-RAW-001)
NOTE: Review Discovery_Sections_Functions_Brainstorm.md for View Raw design
```

---

## YOUR STATUS UPDATES (Write Here)

### [DATE] Sally:
```
(Write your status updates here when you complete tasks or encounter issues)
```

---

## RULES

1. **Check ORCHESTRATOR_HUB.md** before starting any task
2. **Do not modify backend files** (backend/* is Winston's territory)
3. **Shared files** (api.ts, types) - coordinate through Orchestrator first
4. **After completing a task** - update this file AND notify Orchestrator
5. **If blocked** - update this file immediately with blocker details

---

*Maintained by Orchestrator (Claude). Check ORCHESTRATOR_HUB.md for full project status.*
