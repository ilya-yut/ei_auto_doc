# RULE: Document Authority & Anti-Sprawl Protocol

**Version:** 1.0
**Created:** 2025-12-18
**Purpose:** Prevent "first found document" behavior and document multiplication

---

## ⛔ CRITICAL PROBLEM THIS RULE SOLVES

**AI agents grab "the first occasionally met document" related to a subject instead of using the AUTHORITATIVE document.**

This is DANGEROUS because:

- The first found document might be OUTDATED
- The first found document might be DEPRECATED (in JUNK/)
- Multiple documents on same topic = CONFUSION
- Document multiplication = documentation debt

---

## MANDATORY BEHAVIOR: Document Authority Hierarchy

### 🔴 RULE 1: NEVER USE FIRST-FOUND DOCUMENT

**Before using ANY document for guidance:**

1. **STOP** - Do not act on first document found
2. **CHECK** - Is this document in the authoritative hierarchy below?
3. **IF NO** - Find the authoritative document instead
4. **IF YES** - Proceed with that document

**Violation = Potential for incorrect work based on outdated instructions**

---

### 🔴 RULE 2: AUTHORITATIVE DOCUMENT HIERARCHY

**Use documents in THIS ORDER for each task type:**

#### For ANY Task (Always Start Here)

```
FIRST:  llm_handover.md           → Current state, what's working, what's broken
SECOND: .claude/WORKFLOW.md       → Which phase, which documents to use
THIRD:  CLAUDE.md                 → Project structure, conventions
```

#### For Planning

```
1. llm_handover.md → "Current Work in Progress" (check if exists)
2. llm_handover.md → "Roadmap" (check if planned)
3. .claude/WORKFLOW.md → "Phase 1: Planning" sequence
```

#### For Development

```
1. llm_handover.md → Current state (AUTHORITATIVE)
2. .claude/WORKFLOW.md → "Phase 2: Development" sequence
3. CLAUDE.md → Code conventions, key files
4. docs/[frontend|backend]/app_entry/features/[feature]/*.md → REFERENCE ONLY (see Rule 6)
```

#### For Debugging

```
1. llm_handover.md → "Known Issues" section FIRST
2. .claude/WORKFLOW.md → "Phase 3: Debugging" sequence
3. DEPLOYMENT.md → Troubleshooting (if Docker/infra)
```

#### For Testing

```
1. TESTING.md → Test procedures (SINGLE SOURCE)
2. .claude/WORKFLOW.md → "Phase 4: Testing" sequence
```

#### For Alert Analysis

```
1. docs/th-context/ANALYSIS_WORKFLOW.md → SINGLE SOURCE for analysis logic
2. docs/th-context/analysis-rules/templates/quantitative-alert.yaml → Report template
3. docs/analysis/*.md → Reference examples
```

#### For System Architecture

```
1. docs/th-context/SYSTEM_REFERENCE.md → SINGLE SOURCE for architecture
```

#### For Deployment

```
1. DEPLOYMENT.md → Deployment procedures (SINGLE SOURCE)
2. .claude/WORKFLOW.md → "Phase 5: Deployment" sequence
```

---

### 🔴 RULE 3: NEVER CREATE NEW DOCUMENTS - UPDATE EXISTING

**When you need to document something:**

```
┌─────────────────────────────────────────────────────────────────┐
│                                                                 │
│   ❌ WRONG: Create new document (ANALYSIS_XYZ.md, NOTES.md)    │
│   ✅ RIGHT: Update existing authoritative document              │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

**Document Placement Rules:**

| What to Document | Where to Put It | NEVER Create |
|------------------|-----------------|--------------|
| Current state, work in progress | `llm_handover.md` | ❌ STATUS.md |
| Feature requirements | `docs/[fe|be]/features/[feature]/SPEC.md` | ❌ FEATURE_REQUIREMENTS.md |
| Bug investigation | `llm_handover.md` "Known Issues" | ❌ INVESTIGATION.md |
| Test procedures | `TESTING.md` | ❌ TEST_PLAN.md |
| Deployment issues | `DEPLOYMENT.md` | ❌ DEPLOY_NOTES.md |
| Alert analysis | `docs/analysis/[Module]_[Alert]_Analysis.md` | ❌ Root level .md |
| Roadmap/plans | `llm_handover.md` "Roadmap" | ❌ NEXT_STEPS.md |
| Architecture | `CLAUDE.md` | ❌ ARCHITECTURE.md |

---

### 🔴 RULE 4: DOCUMENT COLLISION DETECTION

**Before reading any document, ask yourself:**

1. **Is there a more authoritative document on this topic?**
   - Check the hierarchy above

2. **Is this document in JUNK/ or TEMP/?**
   - If yes, DO NOT USE - find authoritative version

3. **Are there multiple documents about this topic?**
   - If yes, use ONLY the one in the hierarchy above
   - Report the duplication to user

**If unsure:** Default to `llm_handover.md` - it is THE source of truth.

---

### 🔴 RULE 5: ARCHIVE FOLDERS (JUNK & TEMP)

**These folders are ARCHIVES - NEVER DELETE THEIR CONTENTS:**

| Folder | Purpose | When to Move Here |
|--------|---------|-------------------|
| `JUNK/` | Old doc versions, deprecated content | When doc is superseded by newer version |
| `TEMP/` | Temporary artifacts, work-in-progress | Scratch work, experimental docs |

**CRITICAL:** JUNK and TEMP are INTEGRAL parts of the project. They preserve history.

**When reading documents:**

- `JUNK/*` - ARCHIVED versions, do not use for current work
- `TEMP/*` - TEMPORARY work, do not use for current work

**If you find yourself reading from these locations:**

1. STOP immediately
2. Find the authoritative document from the hierarchy
3. Use the archived version only for historical reference

**When to MOVE docs to JUNK:**

- Document has been superseded by a newer version
- Document contains outdated information that's been updated elsewhere
- Feature documentation has been consolidated into a master doc
- Document was a one-time planning artifact that's been executed

**When to MOVE docs to TEMP:**

- Work-in-progress documents not ready for permanent location
- Experimental documentation being tested
- Scratch notes during active development

---

### 🔴 RULE 6: FEATURE DOCS ARE REFERENCE ONLY

**Feature documentation in `docs/[frontend|backend]/app_entry/features/` is REFERENCE material, NOT authoritative.**

```
docs/frontend/app_entry/features/alert-discoveries/
├── 01-alert-summary/
│   ├── SPEC.md      ← REFERENCE: Original requirements
│   ├── CODE.md      ← REFERENCE: Implementation notes
│   └── ANALYSIS.md  ← REFERENCE: Analysis findings
└── ... (same pattern for all features)
```

**WHY:** Feature docs capture point-in-time information. They may be outdated.

**RULES:**

1. **NEVER treat feature docs as current truth** - use `llm_handover.md` instead
2. **Use feature docs for:**
   - Understanding original requirements
   - Historical context
   - Code location hints
3. **DO NOT update feature docs during active development** - update `llm_handover.md`
4. **Periodically move outdated feature docs to JUNK/** when they no longer reflect reality

**For current implementation status → ALWAYS use `llm_handover.md`**

---

## ENFORCEMENT: Self-Check Before Acting

**Before taking action based on ANY document:**

```
□ Is this document in the authoritative hierarchy?
□ Have I checked llm_handover.md FIRST?
□ Am I using the SINGLE authoritative document for this task?
□ Am I NOT in JUNK/, TEMP/, or deprecated location?
□ Will I UPDATE an existing doc (not CREATE a new one)?
```

**If any checkbox is NO → STOP and find correct document.**

---

## WHY THIS RULE EXISTS

**User Statement (2025-12-18):**

> "I presume that you follow the instructions while finding THE FIRST OCCASIONALLY MET DOCUMENT THAT IS RELATED TO THE SUBJECT YOU WORK ON... THIS IS DANGEROUS because the 1st occasional document might NOT BE the document you need to use to execute."

**The Problem:**

- AI grabs first document found
- First document may be outdated, deprecated, or wrong
- Multiple documents on same topic = inconsistent behavior
- New documents created instead of updating existing = document sprawl

**The Solution:**

- STRICT hierarchy of authoritative documents
- NEVER use first-found document without checking hierarchy
- NEVER create new documents - update existing ones
- ALWAYS check llm_handover.md FIRST

---

## RELATED RULES

- `.claude/WORKFLOW.md` - Development phase workflows
- `.claude/rules/llm-handover-maintenance.md` - How to update llm_handover.md
- `.claude/rules/anti-hallucination-rules.md` - Verification before claiming

---

## VIOLATION CONSEQUENCES

- Incorrect work based on outdated instructions
- Wasted user time and money
- Documentation debt accumulation
- Inconsistent project state
- Loss of trust

---

**NO EXCEPTIONS TO THIS RULE.**

*If a document is not in the authoritative hierarchy, it is NOT authoritative.*
