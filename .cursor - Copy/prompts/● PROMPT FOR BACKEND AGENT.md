# PROMPT FOR BACKEND AGENT (Winston)

**Last Updated:** 2026-01-16 10:40
**Updated By:** Orchestrator (Claude)
**Status:** ACTIVE

---

## COORDINATION PROTOCOL

```text
┌─────────────────────────────────────────────────────────────────┐
│  YOU ARE: Winston (Backend Agent)                               │
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
1. .claude/prompts/backend-pre-task.md       → Backend-specific rules & standards
2. .claude/rules/anti-hallucination-rules.md → Verification behavior (18 rules)
3. .claude/rules/preserve-working-code.md    → Git safety rules
4. .claude/rules/document-authority.md       → Document hierarchy
```

**After reading, acknowledge:**
```
"I have read the backend pre-task protocol and all mandatory rules files.
I will verify all changes, follow local development practices, and avoid hallucinations."
```

---

## CURRENT PRIORITY: BE-SOKB-001 (BLOCKING FRONTEND)

### Task: Fix SOKB Rule Logging

**Status:** 🔴 TODO - HIGH PRIORITY

**Problem:** SOKB Classification and Grouping cards are NOT displaying in frontend
because the backend logs rules WITHOUT `input_data` and `output_data`.

**Location:** `backend/app/services/content_analyzer/artifact_reader.py`

**Fix Required:**

#### 1. In `_classify_columns_with_sokb()` method:

```python
self._log_rule_application(
    rule_id="SOKB-CLASS",
    rule_name="SOKB Column Classification",
    rule_source="artifact_reader",
    applied=True,
    triggered=True,
    action_taken=f"Classified {total_columns} columns using SAP Object Knowledge Base",
    input_data={"total_columns": total_columns},  # <-- ADD THIS
    output_data={                                  # <-- ADD THIS
        "classification_counts": classification_counts,
        "characteristics": characteristics_list,
        "key_figures": key_figures_list,
        "time_fields": time_fields_list,
        "currency_fields": currency_fields_list,
        "unknown_fields_excluded": unknown_count
    }
)
```

#### 2. In `_compute_dimension_groupings()` method:

```python
self._log_rule_application(
    rule_id="SOKB-GROUP",
    rule_name="SOKB Dimension Groupings",
    rule_source="artifact_reader",
    applied=True,
    triggered=True,
    action_taken=f"Computed {len(dimensions)} dimension groupings across {total_records} records",
    input_data={                                   # <-- ADD THIS
        "total_records": total_records,
        "characteristic_columns": len(characteristics),
        "time_columns": len(time_cols),
        "currency_column": currency_col,
        "amount_column": amount_col
    },
    output_data={                                  # <-- ADD THIS
        "dimensions_computed": list(dimensions.keys()),
        "dimension_summary": dimension_summary,
        "total_unique_entities": total_unique
    }
)
```

**Verification After Fix:**

1. Upload a NEW alert for fresh analysis
2. Open Actions Log → Processing Flow tab → READ stage → Results tab
3. Confirm SOKB Classification card shows column type counts
4. Confirm SOKB Grouping card shows dimension summaries
5. Update this file with "DONE" and notify Orchestrator

---

## YOUR TASK QUEUE

### Priority 0 (Do Now)

| Task ID | Description | Status | Blocks |
|---------|-------------|--------|--------|
| BE-SOKB-001 | Fix SOKB rule logging (add input_data/output_data) | 🔴 TODO | FE-SOKB-001, FE-SOKB-002 |
| BE-RAW-001 | Investigate 51K row fetch failure | 🔴 TODO | All raw data features |

### Priority 1 (After P0)

| Task ID | Description | Status | Depends On |
|---------|-------------|--------|------------|
| BE-RAW-002 | Implement paginated raw data endpoint | 📋 PLANNED | BE-RAW-001 |
| BE-RAW-003 | Add row tracking (contributing_rows per discovery) | 📋 PLANNED | Investigation |
| BE-ANALYZE-001 | Implement pivot aggregation API | 📋 PLANNED | BE-RAW-003 |
| BE-PIPE-001 | Add pipeline_progress column to alert_analyses | 📋 PLANNED | - |

### Priority 2 (Later)

| Task ID | Description | Status | Depends On |
|---------|-------------|--------|------------|
| BE-ANALYZE-002 | Add DuckDB for server-side aggregation | 📋 PLANNED | BE-ANALYZE-001 |
| BE-PIPE-002 | Persist progress on analysis completion | 📋 PLANNED | BE-PIPE-001 |
| BE-PIPE-003 | Create /pipeline-history/{id} endpoint | 📋 PLANNED | BE-PIPE-002 |
| BE-PIPE-004 | Create /improvements CRUD endpoints | 📋 PLANNED | BE-PIPE-001 |

---

## INVESTIGATION: BE-RAW-001 (51K Row Fetch Failure)

**Context:** User reported that fetching raw data with 51K+ rows consistently fails.

**Questions to Answer:**

1. Where does the "Output" button fetch data from?
2. Is it a frontend timeout, backend memory issue, or JSON serialization problem?
3. What's the current implementation? Single API call returning all rows?

**Files to Check:**

- `backend/app/api/alert_dashboard.py` - API endpoints
- `backend/app/services/content_analyzer/artifact_reader.py` - Data reading
- `frontend/src/pages/alert-discoveries/features/section-actions/ViewRawModal.tsx` - Frontend

**Recommendations from Design Discussion:**

- Use pagination (100 rows per page)
- Consider DuckDB for aggregation of large datasets
- Never send 51K rows to browser - aggregate or paginate

---

## DESIGN DECISIONS (Already Made)

These decisions were made in brainstorming sessions. **Do not re-decide.**

### Raw Data API

- Paginated endpoint: 100 rows per request
- Support "Go to row" navigation
- Store `contributing_rows` per discovery in PostgreSQL
- Support toggle: "This Discovery" vs "Full Alert"

### Pivot/Analyze API

- Server-side aggregation (DuckDB recommended)
- Return aggregated results only (not raw rows)
- Support nested grouping (up to 3 levels)
- Support measures: Sum, Count, Avg, Min, Max

### Reference Documents

- `Discovery_Sections_Functions_Brainstorm.md` - Full design discussion
- `THA Pipeline Flow Integration Plan.md` - Pipeline API spec

---

## FILES YOU OWN

```text
backend/app/
├── api/
│   ├── alert_dashboard.py         ← Raw data endpoints
│   └── content_analysis.py        ← Pipeline endpoints
├── services/
│   └── content_analyzer/
│       ├── artifact_reader.py     ← BE-SOKB-001 FIX HERE
│       ├── detailed_logger.py     ← Rule logging
│       └── analyzer.py            ← Main orchestration
├── models/
│   └── alert_analysis.py          ← DB models
```

---

## MESSAGES FROM ORCHESTRATOR

```text
[2026-01-16 10:40] ORCHESTRATOR:
PRIORITY: Complete BE-SOKB-001 ASAP - Frontend is blocked
THEN: Investigate BE-RAW-001 (51K failure)
LOCATION: backend/app/services/content_analyzer/artifact_reader.py
METHODS: _classify_columns_with_sokb(), _compute_dimension_groupings()
```

---

## YOUR STATUS UPDATES (Write Here)

### [DATE] Winston

```text
(Write your status updates here when you complete tasks or encounter issues)
```

---

## RULES

1. **Check ORCHESTRATOR_HUB.md** before starting any task
2. **Do not modify frontend files** (frontend/* is Sally's territory)
3. **Shared files** (api.ts types) - coordinate through Orchestrator first
4. **After completing a task** - update this file AND notify Orchestrator
5. **If blocked** - update this file immediately with blocker details
6. **Test your changes** - Verify fix before marking done

---

*Maintained by Orchestrator (Claude). Check ORCHESTRATOR_HUB.md for full project status.*
