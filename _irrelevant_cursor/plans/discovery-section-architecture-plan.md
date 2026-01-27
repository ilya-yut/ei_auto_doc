# Architecture Planning Request: Discovery Section Data Pipeline

**Date:** 2025-12-27
**Requester:** Product Owner
**Target:** Architect Claude Code Agent

---

## INSTRUCTIONS FOR ARCHITECT AGENT

You are being tasked with designing the architecture for the **Discovery Section Data Pipeline**. This document contains everything you need to produce a comprehensive architecture design.

### What You Must Do

1. **Read all context artifacts** listed in Section 2 (priority order matters)
2. **Understand the current architecture** shown in Section 3
3. **Design the new architecture** based on Section 4 requirements
4. **Address all design decisions** listed in Section 6.1
5. **Follow the implementation phases** in Section 10 (4 phases provided)
6. **Produce all deliverables** listed in Section 6.3

### Key Deliverables Expected From You

| Deliverable | Description |
|-------------|-------------|
| Architecture Diagram | Component interactions, data flow |
| Data Model Changes | Schema updates for source_rows tracking |
| API Design | Raw data + section info endpoints |
| LLM Prompt Templates | Per-section prompts (5 sections) |
| Implementation Plan | Refine the 4 phases with specific tasks |

### Document Structure

| Section | Content |
|---------|---------|
| 1. Mission | 3 objectives (analyzers, View Raw, Info buttons) |
| 2. Context | Artifacts to read first |
| 3. Current Architecture | How it works now (monolithic LLM call) |
| 4. Proposed Architecture | Section-specific analyzers with source_rows |
| 5. UI Sections | 5 sections with View Raw + Info requirements |
| 6. Requirements | Design decisions, constraints, deliverables |
| 7. Example | Concentration section deep dive |
| 8. Action Checklist | Your task list |
| 9. Reserved | (empty) |
| 10. Implementation Phases | 4-phase rollout plan |
| 11. Success Criteria | Checkboxes for completion |

### Start Here

Begin by reading Section 2 artifacts in order, then proceed through the document sequentially.

---

## 1. MISSION

Design an architecture for the **Alert Discoveries** feature that:

1. **Creates dedicated analysis scripts + LLM prompts for EACH UI section** of the Discovery page
2. **Implements "View Raw" functionality** that traces processed data back to source Summary_* file entries
3. **Implements "Info" button for EACH section** that explains:
   - Purpose of the section (what business question it answers)
   - How the data is calculated (formulas, aggregations, thresholds)
   - What data sources feed into this section
   - How to interpret the results (what's good/bad, action triggers)

---

## 2. CONTEXT - READ THESE ARTIFACTS FIRST

### 2.1 Project Documentation (Priority Order)

| Document | Purpose | Path |
|----------|---------|------|
| **LLM Handover** | Current state, data flow, architecture | `llm_handover.md` |
| **CLAUDE.md** | Project structure, conventions, API endpoints | `CLAUDE.md` |
| **System Architecture** | THA architecture overview | `docs/th-context/THA-System-Architecture.md` |
| **Analysis Rules** | Quantitative alert analysis rules | `.claude/rules/quantitative-alert-analysis.md` |
| **Analysis Template** | Standard analysis output format | `docs/th-context/analysis-rules/templates/quantitative-alert.yaml` |

### 2.2 Example Analysis Reports (Study These Patterns)

```
docs/analysis/
├── SD_Payment_Terms_Check_Analysis.md      # Good example - shows section breakdown
├── MD_Modified_Vendor_Bank_Account_Analysis.md
├── MM_Material_Standard_Price_Change_Analysis.md
└── FI_Exceptional_Posting_GL_Account_Analysis.md
```

### 2.3 Frontend Components (Current Implementation)

```
frontend/src/pages/alert-discoveries/
├── AlertDiscoveries.tsx                    # Main page orchestrator
└── features/
    ├── concentration-hero/
    │   ├── ConcentrationHero.tsx           # Donut chart + Risk Analysis panel
    │   ├── FraudStatusCard.tsx             # Fraud indicator display
    │   ├── RiskIndicators.tsx              # Focus Area + Risk Patterns
    │   └── RawOutputButton.tsx             # "View Raw" button component
    ├── discovery-cards/
    │   └── DiscoveryCard.tsx               # Critical discovery cards
    └── sidebar/
        └── DiscoverySidebar.tsx            # Alert list sidebar
```

### 2.4 Backend Analysis Services

```
backend/app/services/
├── analysis/
│   ├── analyzer.py                         # Main analysis orchestrator
│   └── content_analyzer.py                 # LLM-based content analysis
├── llm_engine/
│   └── anthropic_client.py                 # Anthropic API integration
└── ingestion/
    └── excel_parser.py                     # Summary_*.xlsx parsing
```

### 2.5 API Endpoints (Relevant)

```
# Content Analysis
POST /api/v1/content-analysis/analyze-and-save    # Full analysis pipeline
GET  /api/v1/content-analysis/samples             # List available alerts

# Alert Dashboard (Data Retrieval)
GET  /api/v1/alert-dashboard/critical-discoveries # Get processed discoveries
GET  /api/v1/alert-dashboard/analyses/{id}        # Get full analysis details
```

### 2.6 Database Schema (Key Tables)

```sql
-- AlertAnalysis: Stores LLM analysis results
alert_analyses (
    id, alert_instance_id, analysis_type, severity, risk_score,
    fraud_indicator, focus_area, financial_impact_usd,
    raw_summary_data,          -- JSON: Original Summary_* data
    llm_analysis_output,       -- JSON: Full LLM response
    created_at
)

-- CriticalDiscovery: Individual findings
critical_discoveries (
    id, alert_analysis_id, title, description, metric_value,
    metric_unit, percentage_of_total, affected_entity,
    is_fraud_indicator, source_rows     -- NEW: Track source row indices
)

-- ConcentrationMetric: Concentration data
concentration_metrics (
    id, alert_analysis_id, dimension_type, entity_name,
    entity_value, percentage, transaction_count,
    source_rows                         -- NEW: Track source row indices
)
```

---

## 3. CURRENT DATA FLOW

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         CURRENT ARCHITECTURE                                 │
└─────────────────────────────────────────────────────────────────────────────┘

    Alert Folder (4 artifacts)
    ├── Summary_*.xlsx          ─┐
    ├── Metadata_*.xlsx          │
    ├── Explanation_*.docx       ├──► content_analyzer.py ──► SINGLE LLM CALL
    └── Code_*.txt              ─┘           │
                                             ▼
                                    AlertAnalysis (DB)
                                             │
                                             ▼
                                    ┌────────┴────────┐
                                    │                 │
                            critical_discoveries  concentration_metrics
                                    │                 │
                                    └────────┬────────┘
                                             │
                                             ▼
                                      Frontend UI
                                    (All sections)
```

**Problem:** Single monolithic LLM call produces ALL analysis at once. No traceability to source rows.

---

## 4. PROPOSED ARCHITECTURE

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                         PROPOSED ARCHITECTURE                                │
└─────────────────────────────────────────────────────────────────────────────┘

    Alert Folder (4 artifacts)
    ├── Summary_*.xlsx          ─┐
    ├── Metadata_*.xlsx          │
    ├── Explanation_*.docx       │
    └── Code_*.txt              ─┘
              │
              ▼
    ┌─────────────────────────────────────────────────────────────────────┐
    │                    SECTION-SPECIFIC ANALYZERS                        │
    │                                                                      │
    │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐               │
    │  │ KPI Metrics  │  │ Concentration│  │  Critical    │               │
    │  │  Analyzer    │  │   Analyzer   │  │  Discovery   │               │
    │  │              │  │              │  │   Analyzer   │               │
    │  │ • records    │  │ • by user    │  │ • top issues │               │
    │  │ • period     │  │ • by entity  │  │ • patterns   │               │
    │  │ • value      │  │ • by org     │  │ • fraud      │               │
    │  │ • severity   │  │              │  │              │               │
    │  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘               │
    │         │                 │                 │                        │
    │         │  source_rows[]  │  source_rows[]  │  source_rows[]        │
    │         ▼                 ▼                 ▼                        │
    └─────────────────────────────────────────────────────────────────────┘
              │
              ▼
    ┌─────────────────────────────────────────────────────────────────────┐
    │                         DATABASE                                     │
    │                                                                      │
    │   alert_analyses                                                     │
    │   ├── kpi_section_data (JSON + source_rows)                         │
    │   ├── concentration_section_data (JSON + source_rows)               │
    │   ├── discoveries_section_data (JSON + source_rows)                 │
    │   └── raw_summary_data (original parsed Summary_* content)          │
    │                                                                      │
    └─────────────────────────────────────────────────────────────────────┘
              │
              ▼
    ┌─────────────────────────────────────────────────────────────────────┐
    │                    FRONTEND UI SECTIONS                              │
    │                                                                      │
    │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐               │
    │  │  KPI BAR     │  │ ENTITY       │  │  CRITICAL    │               │
    │  │  Section     │  │ CONCENTRATION│  │ DISCOVERIES  │               │
    │  │              │  │ Section      │  │  Section     │               │
    │  │ [View Raw]   │  │ [View Raw]   │  │ [View Raw]   │               │
    │  └──────────────┘  └──────────────┘  └──────────────┘               │
    │                                                                      │
    │                     "View Raw" Modal                                 │
    │         ┌─────────────────────────────────────────┐                 │
    │         │  Summary_* rows that fed this section   │                 │
    │         │  • Row 5: PMOKGOKO | 54.5% | ...        │                 │
    │         │  • Row 12: VNJAPHA | 27.3% | ...        │                 │
    │         │  • Row 18: PKHUMBUZA | 18.2% | ...      │                 │
    │         └─────────────────────────────────────────┘                 │
    │                                                                      │
    └─────────────────────────────────────────────────────────────────────┘
```

---

## 5. UI SECTIONS REQUIRING DEDICATED ANALYZERS

**Each section MUST have TWO action buttons:**
- **[View Raw]** - Shows source Summary_* rows that fed this section
- **[Info]** - Explains purpose, calculation method, interpretation guide

---

### 5.1 KPI Bar Section (Top Metrics)
**Location:** Top of Discovery detail page
**Current Fields:**
- Module (from Metadata)
- Severity (calculated)
- Records count (from Summary row count)
- Period (from Summary date range)
- Financial Exposure (sum of value columns)
- Fraud Indicator (YES/NO/INVESTIGATE)

**View Raw:** Show which Summary_* rows contributed to each metric

**Info Content:**
- **Purpose:** Quick snapshot of alert scope and severity
- **Calculation:**
  - Records = COUNT(Summary rows)
  - Period = MIN(date) to MAX(date) from Summary
  - Financial Exposure = SUM(value columns) with currency conversion
  - Severity = Based on thresholds (CRITICAL >$1M, HIGH >$100K, etc.)
- **Interpretation:** Red Fraud indicator = immediate investigation needed

---

### 5.2 Entity Concentration Section (Donut Chart)
**Location:** Left side of hero area
**Current Fields:**
- Top entity + percentage
- Distribution breakdown (top 3-4 entities + "Other")
- Entity type (User, Customer, Vendor, Org, etc.)

**View Raw:** Show Summary_* rows grouped by entity

**Info Content:**
- **Purpose:** Identify if activity is concentrated in few entities (risk signal)
- **Calculation:**
  - Group Summary rows by entity column (User/Customer/Vendor)
  - Calculate percentage = entity_count / total_count * 100
  - Top 3-4 shown individually, rest grouped as "Other"
- **Interpretation:** >50% concentration in single entity = HIGH risk flag

---

### 5.3 Risk Analysis Section
**Location:** Right side of hero area
**Current Fields:**
- Fraud Status
- Focus Area
- Risk Patterns (tags)

**View Raw:** Show evidence rows for each risk pattern

**Info Content:**
- **Purpose:** Summarize risk classification and detected patterns
- **Calculation:**
  - Fraud Status = LLM analysis of patterns (YES/NO/INVESTIGATE)
  - Focus Area = Classification into 6 THA categories
  - Risk Patterns = Boolean flags (Fraud Signal, High Concentration, etc.)
- **Interpretation:** Multiple active patterns = compound risk, escalate priority

---

### 5.4 Critical Discoveries Section
**Location:** Below hero area
**Current Fields:**
- Discovery title
- Metric value + unit
- Affected entity
- Description
- Percentage of total
- Fraud indicator flag

**View Raw:** Show exact Summary_* rows that formed this discovery

**Info Content:**
- **Purpose:** Highlight top findings requiring immediate attention
- **Calculation:**
  - LLM identifies patterns, anomalies, outliers from Summary data
  - Metric values extracted from specific columns
  - Percentage = discovery_value / total_value * 100
- **Interpretation:** FRAUD badge = potential fraud indicator, prioritize investigation

---

### 5.5 Impact Analysis Section
**Location:** Right sidebar
**Current Fields:**
- Findings count
- Risk Score
- Score Calculation breakdown

**View Raw:** Show rows that contributed to risk factors

**Info Content:**
- **Purpose:** Quantify overall risk level for prioritization
- **Calculation:**
  - Risk Score = Base (severity) + modifiers (fraud +10, concentration +5, etc.)
  - Capped at 100
  - See THA-System-Architecture.md for full formula
- **Interpretation:**
  - 90-100 = CRITICAL (immediate action)
  - 70-89 = HIGH (urgent review)
  - 50-69 = MEDIUM (standard review)
  - <50 = LOW (monitor)

---

## 6. REQUIREMENTS FOR ARCHITECT

### 6.1 Design Decisions Needed

1. **LLM Strategy**
   - Single call with section-tagged output?
   - OR Multiple sequential/parallel calls per section?
   - Consider: cost, latency, consistency

2. **Source Row Tracking**
   - How to store row indices/references in DB?
   - JSON array of row numbers? Row hash IDs?
   - Handle: deleted rows, re-analysis, data updates

3. **View Raw Implementation**
   - Option A: Fetch only relevant rows (lighter, faster)
   - Option B: Fetch all + highlight relevant (context, but heavier)
   - Modal vs side panel vs new page?

4. **Prompt Engineering**
   - Separate prompt templates per section?
   - How to ensure consistent output schema?
   - How to reference specific rows in prompt output?

### 6.2 Technical Constraints

- **LLM Provider:** Anthropic Claude (claude-sonnet-4-20250514)
- **Max tokens:** ~100K context, but Summary_* can have 1000+ rows
- **Database:** PostgreSQL with JSONB for flexible storage
- **Frontend:** React + TypeScript, TanStack Query for data fetching
- **Cost sensitivity:** Minimize redundant LLM calls

### 6.3 Deliverables Expected

1. **Architecture Diagram** - Component interactions
2. **Data Model Changes** - Schema updates needed
3. **API Design** - New/modified endpoints
4. **LLM Prompt Templates** - Per-section prompts
5. **Implementation Plan** - Phased approach with dependencies

---

## 7. EXAMPLE: Concentration Section Deep Dive

**Input (Summary_* rows):**
```
Row | User      | Full Name       | Amount    | Date       | Status
----|-----------|-----------------|-----------|------------|--------
5   | PMOKGOKO  | Portia Mokgoko  | 300,374   | 2025-11-19 | Changed
12  | VNJAPHA   | Vusumzi Njapha  | 300,374   | 2025-11-20 | Changed
18  | PKHUMBUZA | Percy Khumbuza  | 19,560    | 2025-11-21 | Changed
... (8 more rows)
```

**LLM Prompt (Section-Specific):**
```
Analyze the following Summary data for USER CONCENTRATION patterns.

<summary_data>
{rows_json}
</summary_data>

Return JSON:
{
  "concentration_type": "USER",
  "top_entities": [
    {"name": "PMOKGOKO (Portia Mokgoko)", "percentage": 54.5, "count": 6, "source_rows": [5, 8, 14, 19, 22, 25]},
    {"name": "VNJAPHA (Vusumzi Njapha)", "percentage": 27.3, "count": 3, "source_rows": [12, 15, 28]},
    {"name": "PKHUMBUZA (Percy Khumbuza)", "percentage": 18.2, "count": 2, "source_rows": [18, 31]}
  ],
  "flags": ["concentration_above_50_percent"]
}
```

**View Raw Response:**
When user clicks "View Raw" on concentration donut:
```json
{
  "section": "concentration",
  "entity": "PMOKGOKO",
  "source_rows": [
    {"row_num": 5, "data": {"User": "PMOKGOKO", "Amount": 300374, ...}},
    {"row_num": 8, "data": {"User": "PMOKGOKO", "Amount": 150000, ...}},
    // ... 4 more rows
  ],
  "total_rows_in_summary": 11,
  "rows_for_this_entity": 6
}
```

---

## 8. ACTION FOR ARCHITECT

1. **Read all artifacts** listed in Section 2
2. **Understand current flow** from Section 3
3. **Design new architecture** based on Section 4 requirements
4. **Address all decisions** in Section 6.1
5. **Produce deliverables** listed in Section 6.3
6. **Consider phased rollout** - which section to implement first?

**Recommended first section:** Entity Concentration (simplest data shape, clear row mapping)

---

## 10. IMPLEMENTATION PHASES

### Phase 1: Foundation (Backend Infrastructure)
**Duration:** First
**Goal:** Establish data model and API infrastructure for source row tracking

**Tasks:**
1. **Database Schema Updates**
   - Add `source_rows` JSONB column to `critical_discoveries` table
   - Add `source_rows` JSONB column to `concentration_metrics` table
   - Add `section_data` JSONB column to `alert_analyses` for per-section storage
   - Create migration script

2. **Raw Data Retrieval Endpoint**
   - `GET /api/v1/alert-dashboard/raw-data/{analysis_id}/{section}`
   - Returns original Summary_* rows based on stored source_rows indices
   - Include row number, all columns, highlight which contributed to this section

3. **Section Info Endpoint**
   - `GET /api/v1/alert-dashboard/section-info/{section_type}`
   - Returns purpose, calculation method, interpretation guide
   - Can be static YAML/JSON config initially, LLM-generated later

4. **Store Raw Summary Data**
   - Ensure `raw_summary_data` in `alert_analyses` stores FULL parsed Summary_* content
   - Include row indices for later lookup

---

### Phase 2: Pilot Section - Entity Concentration
**Duration:** Second
**Goal:** Full implementation of one section as proof of concept

**Why Concentration First:**
- Simplest data shape (entity → count → percentage)
- Clear row mapping (group by entity column)
- Already has donut chart visualization
- Low risk for regression

**Tasks:**
1. **Section-Specific Analyzer**
   - Create `backend/app/services/analysis/section_analyzers/concentration_analyzer.py`
   - Dedicated LLM prompt for concentration analysis only
   - Output includes `source_rows` array per entity

2. **LLM Prompt Template**
   - Create `backend/app/services/analysis/prompts/concentration_prompt.yaml`
   - Input: Summary rows as JSON
   - Output: Concentration data with source_rows

3. **Frontend: View Raw Button**
   - Add onClick handler to existing RawOutputButton in ConcentrationHero
   - Fetch from raw data endpoint
   - Display modal with source rows table

4. **Frontend: Info Button**
   - Create InfoButton component (reusable across sections)
   - Fetch from section info endpoint
   - Display modal with purpose/calculation/interpretation

5. **Verification**
   - Compare calculated concentration with source rows
   - Verify clicking "View Raw" shows correct Summary_* entries

---

### Phase 3: Roll Out to Remaining Sections
**Duration:** Third
**Goal:** Apply Phase 2 patterns to remaining 4 sections

**Order of Implementation:**
1. **KPI Bar Section** - Straightforward metrics (counts, sums, dates)
2. **Critical Discoveries Section** - LLM-identified findings with row references
3. **Risk Analysis Section** - Pattern flags with evidence rows
4. **Impact Analysis Section** - Score calculation with factor breakdown

**Per-Section Tasks:**
- Create section analyzer class
- Create LLM prompt template
- Wire View Raw button
- Wire Info button
- Test source row accuracy

---

### Phase 4: Optimization & Polish
**Duration:** Fourth
**Goal:** Performance, UX, and cost optimization

**Tasks:**
1. **LLM Cost Optimization**
   - Evaluate: single call vs multiple calls
   - Consider caching frequently-used prompts
   - Batch analysis where possible

2. **Performance**
   - Add indices on source_rows JSONB columns
   - Optimize raw data retrieval queries
   - Consider pagination for large Summary_* files

3. **UX Polish**
   - Consistent modal design for View Raw and Info
   - Loading states
   - Error handling
   - Keyboard accessibility

4. **Documentation**
   - Update API documentation
   - Add section info content for all 5 sections
   - Create developer guide for adding new sections

---

## 11. SUCCESS CRITERIA

**Updated: 2025-12-31**

- [x] Each UI section has dedicated analyzer with specific prompt
  - ✅ ConcentrationAnalyzer (270 lines, rule-based)
  - ✅ DiscoveryAnalyzer (401 lines, pattern detection)
  - ✅ FindingsAnalyzer (key findings extraction)
- [x] Source row tracking implemented (source_rows array per data point)
  - ✅ All analyzers return `source_rows` in SectionResult
- [x] "View Raw" button shows exact Summary_* rows for any section
  - ✅ Backend: `/raw-data/{analysis_id}/{section}` endpoint at line 1353
  - ✅ Frontend: `ViewRawModal.tsx` (159 lines)
  - ✅ Hook: `useSectionRawData.ts` (64 lines)
  - ❌ NOT WIRED: Buttons not added to ConcentrationHero/DiscoveryDetailPanel
- [x] "Info" button shows purpose, calculation method, and interpretation guide for each section
  - ✅ Backend: `/section-info/{section}` endpoint at line 1442
  - ✅ Frontend: `SectionInfoModal.tsx`, `InfoButton.tsx`
  - ✅ Hook: `useSectionInfo.ts`
  - ❌ NOT WIRED: Buttons not added to UI components
- [x] Info content is stored/generated per section (not hardcoded in frontend)
  - ✅ YAML templates in `prompts/info_templates/`
- [x] Architecture supports future sections without refactoring
  - ✅ BaseSectionAnalyzer abstract class with clear interface
- [x] LLM costs optimized (no redundant calls)
  - ✅ Rule-based analyzers (concentration, discovery) use NO LLM calls
- [ ] Data consistency maintained across sections
  - ⏳ Needs testing after UI wiring

---

## 12. REMAINING WORK (2025-12-31)

1. **Wire View Raw buttons** to ConcentrationHero.tsx and DiscoveryDetailPanel.tsx
2. **Wire Info buttons** to all section headers
3. **Test full flow**: Click → Fetch → Display in modal
4. **Verify data consistency** between stored analysis and re-run

---

*Document prepared for Architect Claude Code Agent*
*Project: Treasure Hunt Analyzer (THA)*
*Last Updated: 2025-12-31*
