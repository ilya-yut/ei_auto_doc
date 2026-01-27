# ORCHESTRATOR HUB - Multi-Agent Coordination Center

**Last Updated:** 2026-01-16 10:30
**Orchestrator:** Claude (PMO + Architect)
**Status:** ACTIVE

---

## COMMUNICATION PROTOCOL

```
┌─────────────────────────────────────────────────────────────────────────────────┐
│                         3-ARTIFACT COMMUNICATION SYSTEM                          │
├─────────────────────────────────────────────────────────────────────────────────┤
│                                                                                 │
│   ┌─────────────────┐         ┌─────────────────┐         ┌─────────────────┐  │
│   │     SALLY       │         │   ORCHESTRATOR  │         │    WINSTON      │  │
│   │   (Frontend)    │◄───────►│     (Claude)    │◄───────►│    (Backend)    │  │
│   └────────┬────────┘         └────────┬────────┘         └────────┬────────┘  │
│            │                           │                           │           │
│            ▼                           ▼                           ▼           │
│   ┌─────────────────┐         ┌─────────────────┐         ┌─────────────────┐  │
│   │ PROMPT FOR      │         │ ORCHESTRATOR    │         │ PROMPT FOR      │  │
│   │ FRONTEND        │         │ _HUB.md         │         │ BACKEND         │  │
│   │ AGENT.md        │         │ (This file)     │         │ AGENT.md        │  │
│   └─────────────────┘         └─────────────────┘         └─────────────────┘  │
│                                                                                 │
├─────────────────────────────────────────────────────────────────────────────────┤
│  READ RULES:                                                                    │
│  • Sally reads: PROMPT FOR FRONTEND AGENT.md (her tasks)                       │
│  • Winston reads: PROMPT FOR BACKEND AGENT.md (his tasks)                      │
│  • Both agents read: ORCHESTRATOR_HUB.md (coordination, dependencies)          │
│  • USER reads: All 3 files for oversight                                       │
│  • Claude (Orchestrator) maintains: All 3 files                                │
├─────────────────────────────────────────────────────────────────────────────────┤
│  WRITE RULES:                                                                   │
│  • Sally writes status updates to: PROMPT FOR FRONTEND AGENT.md                │
│  • Winston writes status updates to: PROMPT FOR BACKEND AGENT.md               │
│  • Claude updates: ORCHESTRATOR_HUB.md after any agent status change           │
│  • Cross-agent messages go through: ORCHESTRATOR_HUB.md                        │
└─────────────────────────────────────────────────────────────────────────────────┘
```

---

## TASK REGISTRY (Master List)

### Task ID Format: `[AGENT]-[CATEGORY]-[NUMBER]`

- FE = Frontend (Sally)
- BE = Backend (Winston)
- ORCH = Orchestrator (Claude)

---

### FRONTEND TASKS (Sally)

| ID | Task | Priority | Status | Depends On | Notes |
|----|------|----------|--------|------------|-------|
| FE-SOKB-001 | Display SOKB Classification cards in Processing Flow | P0 | ⏸️ BLOCKED | BE-SOKB-001 | Waiting for backend fix |
| FE-SOKB-002 | Display SOKB Grouping cards in Processing Flow | P0 | ⏸️ BLOCKED | BE-SOKB-001 | Waiting for backend fix |
| FE-RAW-001 | Implement "View Raw" button functionality | P1 | 📋 PLANNED | BE-RAW-001 | Needs paginated API |
| FE-RAW-002 | Add pagination UI (100 rows/page) | P1 | 📋 PLANNED | FE-RAW-001 | |
| FE-RAW-003 | Add "Go to row" navigation | P2 | 📋 PLANNED | FE-RAW-002 | |
| FE-ANALYZE-001 | Implement "Analyze" button modal | P1 | 📋 PLANNED | BE-ANALYZE-001 | |
| FE-ANALYZE-002 | Build pivot dropdown selectors UI | P1 | 📋 PLANNED | FE-ANALYZE-001 | |
| FE-ANALYZE-003 | Add nested grouping (multi-dimension) | P2 | 📋 PLANNED | FE-ANALYZE-002 | |
| FE-ANALYZE-004 | Add drill-down from aggregated to raw rows | P2 | 📋 PLANNED | FE-ANALYZE-002, FE-RAW-001 | |
| FE-PIPE-001 | Implement ProcessingFlowTab component | P1 | 📋 PLANNED | BE-PIPE-001 | Per Pipeline Plan |

---

### BACKEND TASKS (Winston)

| ID | Task | Priority | Status | Depends On | Notes |
|----|------|----------|--------|------------|-------|
| BE-SOKB-001 | Fix SOKB rule logging (add input_data/output_data) | P0 | 🔴 TODO | - | **BLOCKING FE-SOKB-001/002** |
| BE-RAW-001 | Investigate 51K row fetch failure | P0 | 🔴 TODO | - | Blocker for large datasets |
| BE-RAW-002 | Implement paginated raw data endpoint | P1 | 📋 PLANNED | BE-RAW-001 | |
| BE-RAW-003 | Add row tracking (contributing_rows per discovery) | P1 | 📋 PLANNED | - | Needs investigation first |
| BE-ANALYZE-001 | Implement pivot aggregation API | P1 | 📋 PLANNED | BE-RAW-003 | POST /api/analyze/pivot |
| BE-ANALYZE-002 | Add DuckDB for server-side aggregation | P1 | 📋 PLANNED | BE-ANALYZE-001 | For 200K row support |
| BE-PIPE-001 | Add pipeline_progress column to alert_analyses | P1 | 📋 PLANNED | - | Per Pipeline Plan |
| BE-PIPE-002 | Persist progress on analysis completion | P1 | 📋 PLANNED | BE-PIPE-001 | |
| BE-PIPE-003 | Create /pipeline-history/{id} endpoint | P1 | 📋 PLANNED | BE-PIPE-002 | |
| BE-PIPE-004 | Create /improvements CRUD endpoints | P2 | 📋 PLANNED | BE-PIPE-001 | |

---

### ORCHESTRATOR TASKS (Claude)

| ID | Task | Priority | Status | Notes |
|----|------|----------|--------|-------|
| ORCH-001 | Create coordination system | P0 | ✅ DONE | This file |
| ORCH-002 | Organize frontend tasks | P0 | ✅ DONE | See above |
| ORCH-003 | Organize backend tasks | P0 | ✅ DONE | See above |
| ORCH-004 | Update llm_handover.md | P1 | 📋 PLANNED | After milestone |
| ORCH-005 | Documentation cleanup | P2 | 📋 PLANNED | Ongoing |

---

## DEPENDENCY GRAPH

```
                    ┌──────────────────────────────────────────────────────────┐
                    │                    CRITICAL PATH                          │
                    └──────────────────────────────────────────────────────────┘

[BE-SOKB-001] ──────► [FE-SOKB-001] ──────► [FE-SOKB-002]
 Fix SOKB logging      Display cards         Display cards
 ⚠️ BLOCKING          ⏸️ BLOCKED            ⏸️ BLOCKED


[BE-RAW-001] ──────► [BE-RAW-002] ──────► [FE-RAW-001] ──────► [FE-RAW-002]
 51K investigation    Paginated API        View Raw btn       Pagination UI
 🔴 TODO              📋 PLANNED           📋 PLANNED         📋 PLANNED
                            │
                            ▼
                    [BE-RAW-003] ──────► [BE-ANALYZE-001] ──────► [FE-ANALYZE-001]
                    Row tracking          Pivot API            Analyze modal
                    📋 PLANNED            📋 PLANNED           📋 PLANNED


[BE-PIPE-001] ──────► [BE-PIPE-002] ──────► [BE-PIPE-003] ──────► [FE-PIPE-001]
 DB column            Persist progress     API endpoint        Frontend tab
 📋 PLANNED           📋 PLANNED           📋 PLANNED          📋 PLANNED
```

---

## CONFLICT LOG

### Conflict #1: SOKB Status Mismatch (Detected 2026-01-16)

| Source | Claimed Status | Actual Status |
|--------|----------------|---------------|
| PROMPT FOR FRONTEND AGENT.md | "COMPLETE ✅" | INCORRECT |
| PROMPT FOR BACKEND AGENT.md | "NEEDS FIX ⚠️" | CORRECT |

**Resolution:** Backend fix (BE-SOKB-001) must be completed first. Frontend prompts updated.

---

## ACTIVE SPRINT

**Sprint Goal:** Complete SOKB fix + Raw Data investigation

| Priority | Task | Owner | Target |
|----------|------|-------|--------|
| P0 | BE-SOKB-001: Fix SOKB logging | Winston | ASAP |
| P0 | BE-RAW-001: Investigate 51K failure | Winston | ASAP |
| P0 | FE-SOKB-001/002: Unblock after BE-SOKB-001 | Sally | After fix |

---

## MESSAGE QUEUE

### To Frontend (Sally)

```
[2026-01-16 10:30] ORCHESTRATOR:
- WAIT for BE-SOKB-001 completion before testing SOKB cards
- DO NOT mark SOKB as complete until verified with fresh upload
- NEXT TASK after SOKB: Prepare for View Raw implementation
```

### To Backend (Winston)

```
[2026-01-16 10:30] ORCHESTRATOR:
- PRIORITY: Fix BE-SOKB-001 (add input_data/output_data to _log_rule_application)
- THEN: Investigate BE-RAW-001 (51K fetch failure)
- LOCATION: backend/app/services/content_analyzer/artifact_reader.py
```

### From User

```
[2026-01-16] USER:
- Orchestrator system established
- Claude is PMO + Architect + Coordinator
- All 3 prompts must stay synchronized
```

---

## DOCUMENT INVENTORY (What's Authoritative)

| Document | Purpose | Owner |
|----------|---------|-------|
| `ORCHESTRATOR_HUB.md` | Task coordination, dependencies | Claude |
| `PROMPT FOR FRONTEND AGENT.md` | Sally's current task + instructions | Claude → Sally |
| `PROMPT FOR BACKEND AGENT.md` | Winston's current task + instructions | Claude → Winston |
| `llm_handover.md` | Project state, changelog | Claude (after milestones) |
| `Discovery_Sections_Functions_Brainstorm.md` | Design decisions (View Raw, Analyze) | Reference |
| `THA Pipeline Flow Integration Plan.md` | Pipeline implementation plan | Reference |

---

## RULES FOR ALL AGENTS

1. **Before modifying ANY file:**
   - Check ORCHESTRATOR_HUB.md for conflicts
   - Verify task is not blocked by another agent's work

2. **After completing ANY task:**
   - Update your prompt file with status
   - Notify orchestrator (Claude) for hub update

3. **If you encounter a blocker:**
   - Update your prompt file immediately
   - Flag in ORCHESTRATOR_HUB.md MESSAGE QUEUE

4. **Cross-cutting changes (shared files like api.ts):**
   - Coordinate through ORCHESTRATOR_HUB.md first
   - Never assume the other agent isn't working on same file

---

*This document is maintained by Claude (Orchestrator). Last sync: 2026-01-16 10:30*
