# THA Backend Pre-Task Protocol

**Read this BEFORE any backend/API task.**

---

## 1. READ THESE DOCUMENTS IN ORDER (mandatory):

```
.claude/rules/document-authority.md      → Which docs are authoritative
.claude/rules/anti-hallucination-rules.md → Verification rules
.claude/rules/preserve-working-code.md   → Git safety rules
llm_handover.md                          → Current project state
CLAUDE.md                                → Project structure
```

---

## 2. LOCAL DEVELOPMENT ENVIRONMENT

### ❌ BANNED - NEVER SUGGEST:

| Command | Why Banned |
|---------|------------|
| `docker compose up frontend` | Frontend runs LOCALLY with npm |
| `docker compose up backend` | Backend runs LOCALLY with uvicorn |
| `docker compose build frontend` | Frontend is LOCAL, not Docker |
| `docker compose build backend` | Backend is LOCAL, not Docker |
| `docker compose restart frontend` | Frontend is LOCAL |
| `docker compose restart backend` | Backend is LOCAL |

### ✅ REQUIRED - LOCAL DEVELOPMENT SETUP:

| Service | How to Run | Port |
|---------|------------|------|
| **Frontend** | `cd frontend && npm run dev` | http://localhost:3000 |
| **Backend** | `cd backend && uvicorn app.main:app --reload --port 8000` | http://localhost:8000 |
| **PostgreSQL** | `docker compose up -d postgres` (ONLY service in Docker) | localhost:5432 |

### Pre-Development Checklist:

```
□ Is PostgreSQL running? → docker ps | grep postgres
□ Is backend running locally? → Check terminal/process
□ Is frontend running locally? → Check terminal/process
□ Am I editing files in C:\Users\USER\Desktop\tha-new? (NOT worktrees)
```

---

## 3. ANTI-HALLUCINATION RULES (MEMORIZE THESE)

### RULE 1: VERIFY BEFORE CLAIMING

- **NEVER** report that a change was made unless you have **READ THE FILE AFTERWARD**
- **ALWAYS** use Read tool IMMEDIATELY after any edit to verify
- **ONLY** report success after verification shows the change actually exists
- If verification shows change failed, **ADMIT IT IMMEDIATELY**

### RULE 2: NO ASSUMPTIONS AS FACTS

- **NEVER** say "I have implemented" - instead say "I attempted to implement, let me verify"
- **NEVER** claim specific outcomes without reading actual file contents
- **ALWAYS** distinguish between "I tried to do X" and "I successfully completed X"

### RULE 3: MANDATORY VERIFICATION WORKFLOW

```
1. Execute change (Edit tool)
2. IMMEDIATELY Read tool to check actual result
3. Compare actual result with intended change
4. ONLY THEN report what actually happened
5. If change failed, try alternative method and repeat verification
```

### RULE 4: COST CONSCIOUSNESS

- User pays real money for accurate work, not hallucinations
- Wasted iterations due to unverified claims cost real money
- Accuracy on first attempt is more valuable than speed with errors

### RULE 5: NO "YESMAN" BEHAVIOR

- Answer honestly and correctly, NOT "plausibly"
- Truth over politeness - even if disappointing
- Real limitations are more valuable than fake capabilities
- Admit uncertainty instead of making up plausible-sounding answers

### RULE 6: SCREENSHOT EXAMINATION

- When user attaches screenshots, **ACTUALLY LOOK AT THEM**
- Compare screenshot to expected state before claiming anything is fixed
- Screenshots show reality, not assumptions

---

## 4. BACKEND FILE LOCATIONS

### Core Application Structure:

```
backend/
├── app/
│   ├── main.py                    → FastAPI app entry point
│   ├── config.py                  → Configuration/settings
│   ├── database.py                → SQLAlchemy connection
│   ├── api/                       → API endpoints
│   │   ├── alert_dashboard.py     → Dashboard endpoints
│   │   ├── content_analysis.py    → Analysis/SIM endpoints (LARGE FILE)
│   │   ├── data_sources.py        → Upload endpoints
│   │   └── ...
│   ├── models/                    → SQLAlchemy models
│   │   ├── alert_instance.py
│   │   ├── alert_analysis.py
│   │   ├── critical_discovery.py
│   │   └── ...
│   ├── schemas/                   → Pydantic schemas
│   │   ├── alert_dashboard.py
│   │   ├── content_analysis.py
│   │   └── ...
│   └── services/                  → Business logic
│       ├── content_analyzer/      → LLM analysis services
│       │   ├── analyzer.py        → Main analyzer
│       │   ├── llm_classifier.py  → LLM classification
│       │   ├── artifact_reader.py → File parsing
│       │   ├── sim_cycle.py       → SIM orchestration
│       │   ├── sim_discrepancy_detector.py → Discrepancy detection
│       │   └── ...
│       └── ingestion/             → Data ingestion
│           ├── excel_parser_4c.py → Excel parsing
│           └── ...
└── alembic/                       → Database migrations
```

### Key Configuration Files:

| File | Purpose |
|------|---------|
| `backend/app/config.py` | Environment variables, settings |
| `backend/alembic.ini` | Migration configuration |
| `backend/requirements.txt` | Python dependencies |

---

## 5. BACKEND CODING STANDARDS

### Python Style:

```python
# ✅ CORRECT: Type hints, docstrings, logging
from typing import Optional, List, Dict, Any
import logging

logger = logging.getLogger(__name__)

def process_data(
    data: Dict[str, Any],
    threshold: float = 0.5
) -> Optional[List[str]]:
    """
    Process data with threshold filtering.

    Args:
        data: Input data dictionary
        threshold: Minimum value threshold

    Returns:
        List of processed items or None if empty
    """
    logger.info(f"Processing data with threshold {threshold}")
    # Implementation...
```

### FastAPI Endpoints:

```python
# ✅ CORRECT: Response model, error handling, logging
@router.get("/items/{item_id}", response_model=ItemResponse)
async def get_item(item_id: int, db: Session = Depends(get_db)):
    """Get item by ID."""
    try:
        item = db.query(Item).filter(Item.id == item_id).first()
        if not item:
            raise HTTPException(status_code=404, detail=f"Item {item_id} not found")
        return item
    except HTTPException:
        raise
    except Exception as e:
        logger.error(f"Failed to get item {item_id}: {e}")
        raise HTTPException(status_code=500, detail=str(e))
```

### SQLAlchemy Models:

```python
# ✅ CORRECT: Relationships, indexes, constraints
class AlertAnalysis(Base):
    __tablename__ = "alert_analyses"

    id = Column(Integer, primary_key=True, index=True)
    alert_instance_id = Column(Integer, ForeignKey("alert_instances.id"), nullable=False)
    severity = Column(String(20), default="MEDIUM")
    created_at = Column(DateTime, default=datetime.utcnow)

    # Relationships
    alert_instance = relationship("AlertInstance", back_populates="analyses")
    discoveries = relationship("CriticalDiscovery", back_populates="alert_analysis")
```

### Pydantic Schemas:

```python
# ✅ CORRECT: Validation, optional fields, examples
class AnalysisRequest(BaseModel):
    directory_path: str = Field(..., description="Path to alert artifacts")
    force_reanalysis: bool = Field(False, description="Force re-analysis")

    class Config:
        json_schema_extra = {
            "example": {
                "directory_path": "/data/alerts/SD_001",
                "force_reanalysis": False
            }
        }
```

---

## 6. DATABASE OPERATIONS

### Migration Workflow:

```bash
# Create migration
cd backend
alembic revision --autogenerate -m "description"

# Apply migration
alembic upgrade head

# Rollback one step
alembic downgrade -1

# Check current version
alembic current
```

### Query Best Practices:

```python
# ✅ CORRECT: Eager loading, filtering
analysis = db.query(AlertAnalysis).options(
    joinedload(AlertAnalysis.discoveries),
    joinedload(AlertAnalysis.alert_instance)
).filter(
    AlertAnalysis.id == analysis_id
).first()

# ❌ WRONG: N+1 queries
analysis = db.query(AlertAnalysis).first()
for d in analysis.discoveries:  # Triggers separate query each time
    process(d)
```

---

## 7. TESTING PROCEDURES

### How to Test Backend Changes:

```bash
# 1. Ensure postgres is running
docker ps | grep postgres

# 2. Run backend locally (if not running)
cd backend
uvicorn app.main:app --reload --port 8000

# 3. Test endpoint directly
curl http://localhost:8000/api/v1/your-endpoint

# 4. Check logs in terminal where uvicorn is running

# 5. Use Swagger UI for interactive testing
# Open: http://localhost:8000/docs
```

### Visual Verification (with Playwright):

```
1. Make backend change
2. Restart local uvicorn (or wait for auto-reload)
3. Use Playwright to navigate to frontend page
4. Take screenshot
5. Verify change is reflected in UI
```

---

## 8. SIM (Self-Improvement Module) SPECIFIC RULES

### SIM Architecture:

```
content_analysis.py
├── /sim/detect/{analysis_id}    → Detect discrepancies
├── /sim/cycle/{analysis_id}     → Run improvement cycle
└── /sim/pipeline/{analysis_id}  → Get pipeline stages

sim_discrepancy_detector.py      → Detection logic
sim_cycle.py                     → Orchestration logic
sim_corrector.py                 → Correction generation
improvement_rules_loader.py      → YAML rules loading
```

### When Modifying SIM:

1. **Ensure consistency** between `/sim/detect` and `/sim/cycle` endpoints
2. **Both must use same llm_output structure** (check for missing fields!)
3. **Verify accuracy calculations** are correct
4. **Test both endpoints** after changes
5. **Update improvement-rules.yaml** if adding new rules

---

## 9. DOCUMENT AUTHORITY HIERARCHY

### For ANY Backend Task:

```
FIRST:  llm_handover.md           → Current state, what's working
SECOND: .claude/WORKFLOW.md       → Which phase, which documents
THIRD:  CLAUDE.md                 → Project structure, conventions
```

### NEVER Create New Documents:

| What to Document | Where to Put It |
|------------------|-----------------|
| Current state | `llm_handover.md` |
| Bug investigation | `llm_handover.md` "Known Issues" |
| API changes | `llm_handover.md` + update CLAUDE.md |
| Test procedures | `TESTING.md` |

---

## 10. GIT SAFETY RULES

### Before ANY Git Operation:

```
□ Run git status first
□ Check for uncommitted changes
□ NEVER run git checkout on modified files without backup
□ ASK USER before destructive operations (reset, clean)
```

### After Completing Work:

```
□ Update llm_handover.md with changes made
□ Suggest committing to user
□ Document what was changed and why
```

---

## 11. PRE-IMPLEMENTATION CHECKLIST

```
□ Have I read the file I'm about to modify?
□ Am I editing in C:\Users\USER\Desktop\tha-new? (NOT worktrees)
□ Is this a LOCAL development task? (NO Docker for backend/frontend)
□ Do I understand the current state from llm_handover.md?
□ Will I verify my changes by reading files after editing?
□ Am I following the document authority hierarchy?
```

---

## 12. POST-IMPLEMENTATION CHECKLIST

```
□ Did I READ the file after editing to verify the change?
□ Did I test the endpoint/feature locally?
□ Did I check for consistency (e.g., SIM detect vs cycle)?
□ Did I update llm_handover.md?
□ Am I reporting FACTS (verified) not ASSUMPTIONS?
```

---

## 13. COMMON MISTAKES TO AVOID

| Mistake | Why It's Wrong | Correct Behavior |
|---------|----------------|------------------|
| Suggesting Docker for frontend/backend | They run locally | Use npm/uvicorn locally |
| Claiming "change made" without verifying | May have failed silently | Read file after edit |
| Using first-found document | May be outdated | Follow authority hierarchy |
| Editing in worktree path | Docker won't see changes | Edit in main project dir |
| Missing field in endpoint consistency | Causes different behavior | Check all similar endpoints |
| Not testing after changes | Bug may exist | Test with curl/Swagger/UI |

---

## 14. IF UNCERTAIN

- **ASK** before implementing
- **READ** the file first
- **VERIFY** changes after making them
- **NEVER** guess or assume success
- **ADMIT** when something failed

---

## ACKNOWLEDGMENT REQUIRED

Before proceeding with any backend task, state:

1. "I have read the backend pre-task protocol"
2. Confirm LOCAL development (no Docker for backend/frontend)
3. List files that will be modified
4. Confirm verification workflow will be followed

---

*Last Updated: 2026-01-02*
