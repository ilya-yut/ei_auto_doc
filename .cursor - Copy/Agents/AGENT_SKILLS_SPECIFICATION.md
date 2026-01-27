# AGENT SKILLS SPECIFICATION
## Universal CIDRA Framework

**Version**: 1.0.0
**Date**: January 2025
**Purpose**: Define skills for THE_CHUNKER, THE_DOCUMENTER, and THE_RECOMMENDER agents

---

## Overview

This document defines the complete skills specification for three agents in the CIDRA framework:

| Agent | Primary Role | Key Output |
|-------|--------------|------------|
| **THE_CHUNKER** | Prepare code for LLM consumption | Chunked files + Instructive Prompt for Documenter |
| **THE_DOCUMENTER** | Generate zero-hallucination documentation | 7-file documentation set |
| **THE_RECOMMENDER** | Provide modernization recommendations | Recommendation reports via dialog |

### Key Principles

1. **Universal Framework**: Supports any programming language (SAP, C++, SQL, Python, COBOL, etc.)
2. **Multilingual Output**: Documentation can be in any language (English, Hebrew, etc.)
3. **Agent Pipeline**: Chunker → Documenter → Recommender (with user involvement)
4. **Zero Hallucinations**: All documentation based solely on actual code
5. **User Dialog**: Agents ask users for preferences and directions

---

## Skill Categories

```
┌─────────────────────────────────────────────────────────────┐
│                    SKILL CATEGORIES                          │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  1. USER-INVOKED SKILLS (Slash commands)                    │
│     └── /chunk, /document, /recommend                       │
│     └── User types to start agent workflow                  │
│                                                              │
│  2. DIALOG SKILLS (Interactive questioning)                 │
│     └── Ask user about format, scope, preferences           │
│     └── Propose options, get confirmation                   │
│                                                              │
│  3. INTERNAL SKILLS (Automatic capabilities)                │
│     └── Read files, count lines, detect patterns            │
│     └── Agent uses these without user intervention          │
│                                                              │
│  4. OUTPUT SKILLS (Producing results)                       │
│     └── Generate files, export formats                      │
│     └── Create reports and prompts                          │
│                                                              │
│  5. QUALITY SKILLS (Validation & assurance)                 │
│     └── Check for hallucinations, verify counts             │
│     └── Score quality, detect issues                        │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

# 🔪 THE_CHUNKER_AGENT

## Mission
Create optimized code chunks for LLM consumption and generate an instructive prompt that tells THE_DOCUMENTER exactly how to process the chunks.

## Agent Documentation Location
`C:\Users\iliya\OneDrive\Desktop\maccabi_cidra\Agents\THE_CHUNKER_AGENT\`

---

## User-Invoked Skills

### SKILL: `/chunk`
**Type**: User-Invoked (Main Entry Point)
**Command**: `/chunk [path]`
**Purpose**: Chunk code at specified path

**Workflow**:
1. Scan target directory
2. Detect programming languages
3. Apply appropriate chunking strategy
4. Generate metadata for each chunk
5. Build relationship graph
6. Create instructive prompt for Documenter
7. Save all outputs

**Parameters**:
| Parameter | Required | Description |
|-----------|----------|-------------|
| `path` | Yes | Directory or file to chunk |
| `--strategy` | No | Override auto-detected strategy |
| `--output` | No | Output directory (default: `./CHUNKS/`) |

**Example**:
```
/chunk WD/
/chunk "Source Code/ABAP/" --strategy method_level
```

---

### SKILL: `/chunk:analyze`
**Type**: User-Invoked (Discovery)
**Command**: `/chunk:analyze [path]`
**Purpose**: Pre-analysis before chunking

**Output**:
- File inventory (count, types, sizes)
- Language detection results
- Recommended chunking strategy
- Estimated chunk count
- Identified shared components

**Example**:
```
/chunk:analyze "Source Code/"

Output:
├── Total files: 789
├── Languages: ABAP (656), WebDynpro (133)
├── Recommended strategy: method_level
├── Estimated chunks: ~1,200
└── Shared components: 8 detected
```

---

### SKILL: `/chunk:status`
**Type**: User-Invoked (Monitoring)
**Command**: `/chunk:status`
**Purpose**: Show chunking progress and statistics

**Output**:
- Current chunking progress
- Chunks created
- Errors/warnings
- Time elapsed

---

## Internal Skills

### SKILL: Language Detection
**Type**: Internal (Automatic)
**Purpose**: Identify programming language from file patterns and content

**Supported Languages**:
- SAP ABAP (*.abap, *.txt with ABAP patterns)
- WebDynpro ABAP (*_S_*.txt, *_B_*.txt)
- C++ (*.cpp, *.h, *.hpp)
- Python (*.py)
- SQL (*.sql, *.prc, *.fnc)
- COBOL (*.cbl, *.cob)
- RPG/AS400 (*.rpgle, *.sqlrpgle)
- JavaScript/TypeScript (*.js, *.ts, *.jsx, *.tsx)

**Detection Method**:
1. File extension matching
2. Content pattern analysis
3. Keyword detection

---

### SKILL: Structure Parsing
**Type**: Internal (Automatic)
**Purpose**: Parse code structure to identify logical units

**Detects**:
- Functions/Methods
- Classes
- Procedures
- Modules
- Data structures
- Interfaces

**Language-Specific Parsers**:
- ABAP: METHOD/ENDMETHOD, FUNCTION/ENDFUNCTION, CLASS/ENDCLASS
- C++: function definitions, class definitions
- Python: def, class
- SQL: CREATE PROCEDURE, CREATE FUNCTION

---

### SKILL: Boundary Detection
**Type**: Internal (Automatic)
**Purpose**: Find optimal chunk boundaries

**Rules**:
1. Never split mid-function
2. Keep related code together
3. Respect logical units
4. Target 2,000-4,000 tokens per chunk
5. Include context (imports, type definitions)

---

### SKILL: Token Counting
**Type**: Internal (Automatic)
**Purpose**: Count tokens for LLM context optimization

**Thresholds**:
- Minimum: 500 tokens
- Target: 2,000 tokens
- Maximum: 8,000 tokens
- Warning: < 300 or > 10,000

---

### SKILL: Metadata Generation
**Type**: Internal (Automatic)
**Purpose**: Generate rich metadata for each chunk

**Metadata Fields**:
```yaml
chunk_id: "chunk_001"
type: "method" | "class" | "file" | "semantic_group"
file: "source_file.ext"
language: "ABAP"
tokens: 2150
summary: "One-sentence description"
tags: ["validation", "patient", "data_retrieval"]
dependencies: ["other_function", "external_lib"]
used_by: ["caller_function"]
complexity: "low" | "medium" | "high"
```

---

### SKILL: Relationship Mapping
**Type**: Internal (Automatic)
**Purpose**: Map dependencies between chunks

**Maps**:
- Call relationships (who calls whom)
- Data flow (input → processing → output)
- Inheritance (parent/child classes)
- Includes/imports

---

### SKILL: Index Creation
**Type**: Internal (Automatic)
**Purpose**: Create searchable index of all chunks

**Index Structure**:
```
CHUNKS/
├── index.json           # Master index
├── by_language/         # Grouped by language
├── by_complexity/       # Grouped by complexity
├── by_tag/              # Grouped by semantic tags
└── relationships.json   # Relationship graph
```

---

## Output Skills

### SKILL: Chunk Export
**Type**: Output
**Purpose**: Save chunks to files

**Formats**:
- JSON (default)
- Markdown
- Vector DB format (Pinecone, Chroma)

---

### SKILL: Prompt Generation (CRITICAL)
**Type**: Output
**Purpose**: Create instructive prompt for THE_DOCUMENTER

**Output File**: `CHUNKS/DOCUMENTER_INSTRUCTIONS.md`

**Template**:
```markdown
# DOCUMENTER INSTRUCTIONS
Generated by: THE_CHUNKER_AGENT
Project: [PROJECT_NAME]
Date: [DATE]
Language: [DETECTED_LANGUAGE]

---

## 1. WHERE TO LOOK

| Location | Description |
|----------|-------------|
| Chunk directory | [PATH] |
| Total chunks | [N] files |
| Languages found | [LIST] |
| Index file | [PATH]/index.json |

---

## 2. HOW TO APPROACH

### Entry Points
Start documentation with these core chunks:
1. [CHUNK_ID] - [DESCRIPTION]
2. [CHUNK_ID] - [DESCRIPTION]

### Core Components
| Component | Chunk IDs | Purpose |
|-----------|-----------|---------|
| [NAME] | chunk_001, chunk_002 | [PURPOSE] |

### Shared Elements (CRITICAL)
These elements are shared across multiple components:
| Element | Location | Used By |
|---------|----------|---------|
| [NAME] | [CHUNK_ID] | [LIST] |

---

## 3. HOW TO READ

### Chunk Naming Convention
- Pattern: `chunk_[NUMBER]_[TYPE]_[NAME]`
- Example: `chunk_042_method_validate_patient`

### Metadata Location
- Each chunk has `.meta.json` file
- Summary in `summary` field
- Dependencies in `dependencies` field

### Relationship Navigation
- See `relationships.json` for call graph
- Use `used_by` to find callers
- Use `dependencies` to find callees

---

## 4. KEY PATTERNS DETECTED

| Pattern | Description | Chunks |
|---------|-------------|--------|
| [PATTERN_1] | [DESCRIPTION] | chunk_001, chunk_005 |
| [PATTERN_2] | [DESCRIPTION] | chunk_010, chunk_015 |

---

## 5. CROSS-REFERENCES

### Shared Components
These components are referenced by multiple chunks:

| Component | Primary Location | Referenced By |
|-----------|------------------|---------------|
| [NAME] | [CHUNK_ID] | [LIST] |

### Check These Files for Shared Elements
- [FILE_1]: Contains shared [ELEMENT_TYPE]
- [FILE_2]: Contains shared [ELEMENT_TYPE]

---

## 6. DOCUMENTATION RECOMMENDATIONS

Based on analysis, recommend documenting in this order:
1. [COMPONENT_1] - [REASON]
2. [COMPONENT_2] - [REASON]
3. [COMPONENT_3] - [REASON]

---

## 7. WARNINGS & NOTES

- [WARNING_1]
- [NOTE_1]

---

*Generated automatically by THE_CHUNKER_AGENT v1.0*
```

---

## Quality Skills

### SKILL: Chunk Validation
**Type**: Quality
**Purpose**: Validate chunk quality before export

**Checks**:
- Size within limits (500-8,000 tokens)
- No broken syntax (complete functions)
- Metadata complete
- Relationships mapped

---

# 📝 THE_DOCUMENTER_AGENT

## Mission
Generate 100% accurate, zero-hallucination documentation based on actual code analysis. Always ask users about format preferences and produce multilingual documentation.

## Agent Documentation Location
`C:\Users\iliya\OneDrive\Desktop\maccabi_cidra\Agents\THE_DOCUMENTER_AGENT\`

---

## User-Invoked Skills

### SKILL: `/document`
**Type**: User-Invoked (Main Entry Point)
**Command**: `/document [component]`
**Purpose**: Document a specific component

**Workflow**:
1. Read Chunker's instructive prompt (if available)
2. Check if project setup exists
3. If no setup → invoke `/document:setup` first
4. Scan relevant files
5. Count elements EXACTLY
6. Detect cross-references
7. Generate 7-file documentation
8. Validate for hallucinations
9. Report quality score

**Parameters**:
| Parameter | Required | Description |
|-----------|----------|-------------|
| `component` | Yes | Component name to document |
| `--template` | No | Override template |
| `--language` | No | Output language (default: from setup) |

**Example**:
```
/document V_APPROVE
/document V_DETAIL --language hebrew
```

---

### SKILL: `/document:setup`
**Type**: User-Invoked (Project Setup)
**Command**: `/document:setup`
**Purpose**: One-time project setup - establish documentation format

**Dialog Flow**:
```
DOCUMENTER: I'll help you set up documentation standards for this project.

QUESTION 1: What is your preferred documentation language?
  [ ] English only
  [ ] Hebrew only
  [ ] Hebrew + English (bilingual)
  [ ] Other: ___________

QUESTION 2: What documentation template do you prefer?
  [ ] Enterprise 7-file structure (recommended)
      - 01_SCREEN_SPECIFICATION.md
      - 02_UI_MOCKUP.md
      - 03_TECHNICAL_ANALYSIS.md
      - 04_BUSINESS_LOGIC.md
      - 05_CODE_ARTIFACTS.md
      - README.md
      - Validation_Report.md
  [ ] Compact 5-file structure
  [ ] Quick 3-file structure
  [ ] Custom (I'll define)

QUESTION 3: Where should documentation be saved?
  Default: ./Screens/[COMPONENT_NAME]/
  Custom: ___________

QUESTION 4: What naming convention for validation reports?
  [ ] validation_report_[COMPONENT].md
  [ ] דוח_אימות_[COMPONENT].md (Hebrew)
  [ ] [COMPONENT]_validation.md

Saving project setup to: ./DOCUMENTER_PROJECT_CONFIG.yaml
```

**Output**: `DOCUMENTER_PROJECT_CONFIG.yaml`
```yaml
project_name: "[PROJECT]"
setup_date: "[DATE]"
documentation_language: "hebrew+english"
template: "enterprise_7_file"
output_directory: "./Screens/{component_name}/"
validation_report_naming: "דוח_אימות_{component}.md"
quality_threshold: 100
```

---

### SKILL: `/document:validate`
**Type**: User-Invoked (Quality Check)
**Command**: `/document:validate [path]`
**Purpose**: Run validation on existing documentation

**Checks Performed**:
1. File count (expected: 7)
2. Forbidden words scan
3. Careful language presence
4. Estimate language detection
5. Limitations section presence
6. Cross-reference marking
7. Line count accuracy
8. Element count accuracy

**Output**: Quality score (0-100) + detailed report

---

### SKILL: `/document:fix`
**Type**: User-Invoked (Auto-Repair)
**Command**: `/document:fix [path]`
**Purpose**: Auto-fix common documentation issues

**Fixes**:
- Remove forbidden words
- Add careful language where missing
- Add limitations section
- Mark cross-references

---

## Dialog Skills

### SKILL: Format Dialog
**Type**: Dialog
**Purpose**: Ask user about documentation format preferences
**When**: During `/document:setup`

**Questions**:
1. Documentation language
2. Template selection
3. Output directory
4. Naming conventions

---

### SKILL: Override Dialog
**Type**: Dialog
**Purpose**: Propose default format, allow override
**When**: During `/document` if setup exists

**Flow**:
```
DOCUMENTER: Using project defaults:
  - Template: Enterprise 7-file
  - Language: Hebrew + English
  - Output: ./Screens/V_APPROVE/

Do you want to override any settings for this component?
  [ ] No, use defaults (proceed)
  [ ] Yes, let me customize
```

---

### SKILL: Clarification Dialog
**Type**: Dialog
**Purpose**: Ask user when code meaning is ambiguous
**When**: During documentation if uncertain

**Example**:
```
DOCUMENTER: I found a method called `process_special_case`.
The code logic is unclear. What should I document?

  [ ] Document only what's visible in code (safe)
  [ ] Skip this method
  [ ] Let me explain: ___________
```

---

## Internal Skills

### SKILL: Prompt Reading
**Type**: Internal (Automatic)
**Purpose**: Read and interpret Chunker's instructive prompt

**Reads**:
- `CHUNKS/DOCUMENTER_INSTRUCTIONS.md`
- Extracts: where to look, how to approach, cross-references

---

### SKILL: File Scanning
**Type**: Internal (Automatic)
**Purpose**: Scan all files, identify relevant ones

**Process**:
1. Scan all files in project
2. Search for component name
3. Report: "Scanned 133 files → 7 relevant"

---

### SKILL: Exact Counting
**Type**: Internal (Automatic)
**Purpose**: Count lines, methods, nodes EXACTLY

**Commands Used**:
```bash
# Line count (Windows)
(Get-Content [FILE]).Count

# Line count (Linux/Mac)
wc -l [FILE]

# Method count (ABAP)
grep -c "METHOD" [FILE]

# Element count (manual verification)
count field-by-field
```

**Rule**: NEVER estimate. ALWAYS use exact count commands.

---

### SKILL: Cross-Reference Detection
**Type**: Internal (Automatic)
**Purpose**: Identify shared vs unique elements

**Process**:
1. Read Chunker's shared elements list
2. For each element, check if in shared list
3. Mark as "(shared from [SOURCE]!)" or "(unique)"

**Output Format**:
```markdown
### GT_TAB (shared from Component Controller!)
**Location**: Interface line 18
**Primary definition**: Component Controller (0008) line 382
**Note**: Shared across multiple Views!

### LOCAL_DATA (unique to this component)
**Location**: Interface line 45
```

---

### SKILL: Careful Language Application
**Type**: Internal (Automatic)
**Purpose**: Use hedged, careful language throughout

**Required Phrases** (at least 5 per document):
- English: "appears to", "seems to", "according to code", "based on the"
- Hebrew: "נראה ש", "נראה כי", "לפי הקוד", "מבוסס על"

**Automatic Check**: Count occurrences, warn if < 5

---

### SKILL: Forbidden Word Avoidance
**Type**: Internal (Automatic)
**Purpose**: Avoid marketing/overconfident language

**Forbidden Words**:
| English | Hebrew |
|---------|--------|
| advanced | מתקדם |
| smart | חכם |
| intelligent | אינטליגנטי |
| sophisticated | מתוחכם |
| revolutionary | מהפכני |
| seamless | חלק |
| automatic (unless proven) | אוטומטי |

**Rule**: If detected, remove or replace with factual description.

---

### SKILL: Limitations Identification
**Type**: Internal (Automatic)
**Purpose**: Identify what CAN and CANNOT be known from code

**Always Include**:
```markdown
## Documentation Limitations / מגבלות תיעוד

### What CANNOT be determined from code / מה שלא ניתן לדעת:
- Database table contents
- External API behavior
- Business rule meanings
- Full type definitions
- Runtime behavior

### What IS known from code / מה שכן ידוע:
- Code structure (exact counts)
- Method signatures
- Program flow
- Database operations (which tables)
- UI elements
```

---

## Output Skills

### SKILL: Document Generation
**Type**: Output
**Purpose**: Generate 7-file documentation set

**Files Generated**:
```
[COMPONENT]/
├── 01_SCREEN_SPECIFICATION.md   # Technical specs, Context Nodes, Actions
├── 02_UI_MOCKUP.md              # UI structure, elements, layout
├── 03_TECHNICAL_ANALYSIS.md     # Detailed technical breakdown
├── 04_BUSINESS_LOGIC.md         # Program flow, business rules from code
├── 05_CODE_ARTIFACTS.md         # Key code snippets with line numbers
├── README.md                    # Overview, navigation guide
└── [Validation_Report].md       # Quality verification
```

---

### SKILL: Validation Report Generation
**Type**: Output
**Purpose**: Generate quality validation report

**Report Sections**:
1. Files scanned (e.g., "133 scanned, 7 relevant")
2. Exact line counts (verified with command)
3. Exact element counts
4. Shared vs unique breakdown
5. Forbidden word check (0 found)
6. Careful language check (X occurrences)
7. Quality score (0-100)
8. Certification (100 = ready for delivery)

---

## Quality Skills

### SKILL: 100-Point Validation
**Type**: Quality
**Purpose**: Score documentation quality

**Scoring**:
```
Starting: 100 points

Deductions:
- Forbidden word: -5 per occurrence
- Missing careful language: -10
- Estimate language: -15 per occurrence
- Missing limitations section: -20
- Missing cross-reference marking: -25
- Wrong file count: -30
- Inaccurate line count: -40
- Inaccurate element count: -15
```

**Rule**: Only 100/100 is acceptable for delivery.

---

### SKILL: Hallucination Detection
**Type**: Quality
**Purpose**: Check for invented content

**Checks**:
- Every method mentioned exists in code
- Every count matches actual code
- Every code snippet is from actual file
- No invented business logic

---

# 🎯 THE_RECOMMENDER_AGENT

## Mission
Analyze documented code and provide modernization recommendations through interactive dialog with user. Always ask for direction before making recommendations.

## Agent Documentation Location
`C:\Users\iliya\OneDrive\Desktop\maccabi_cidra\Agents\THE_RECOMMENDER_AGENT\`

---

## User-Invoked Skills

### SKILL: `/recommend`
**Type**: User-Invoked (Main Entry Point)
**Command**: `/recommend [component]`
**Purpose**: Start recommendation dialog for a component

**Workflow**:
1. Greet user, explain what's possible
2. Ask direction questions (ALWAYS)
3. Analyze documentation
4. Generate recommendations based on user's direction
5. Present options
6. Confirm with user
7. Generate final report

**Parameters**:
| Parameter | Required | Description |
|-----------|----------|-------------|
| `component` | Yes | Component to analyze |
| `--quick` | No | Skip detailed dialog, use defaults |

---

### SKILL: `/recommend:compare`
**Type**: User-Invoked (Technology Comparison)
**Command**: `/recommend:compare [tech1] [tech2]`
**Purpose**: Compare two technologies side-by-side

**Example**:
```
/recommend:compare ABAP Java
/recommend:compare WebDynpro React
```

---

### SKILL: `/recommend:risk`
**Type**: User-Invoked (Risk Assessment Only)
**Command**: `/recommend:risk [component]`
**Purpose**: Run risk assessment without full recommendation

**Output**: Risk score (0-100) with breakdown

---

## Dialog Skills (CRITICAL FOR THIS AGENT)

### SKILL: Direction Dialog
**Type**: Dialog (ALWAYS FIRST)
**Purpose**: Ask user what kind of recommendations needed
**When**: Always at start of `/recommend`

**Dialog Flow**:
```
RECOMMENDER: I can help you with modernization recommendations for [COMPONENT].

Before I analyze, I need to understand what you're looking for.

QUESTION 1: What type of recommendations do you need?
  [x] Technology alternatives (modern replacements)
  [x] Migration strategy (how to migrate)
  [x] Risk assessment (what could go wrong)
  [x] Cost analysis (investment and ROI)
  [ ] All of the above

QUESTION 2: What are your constraints?
  Budget: [___________] or [ ] Not specified
  Timeline: [___________] or [ ] Not specified
  Team size: [___________] or [ ] Not specified

QUESTION 3: What's your team's current skill set?
  [ ] Strong in [CURRENT_TECH], learning [TARGET_TECH]
  [ ] No experience with modern tech
  [ ] Already skilled in modern tech
  [ ] Other: ___________

QUESTION 4: What's your risk tolerance?
  [ ] Low (prefer safe, incremental changes)
  [ ] Medium (balanced approach)
  [ ] High (willing to take risks for bigger gains)

Thank you! Analyzing [COMPONENT] based on your requirements...
```

---

### SKILL: Options Dialog
**Type**: Dialog
**Purpose**: Present options, get user preference
**When**: After analysis, before final recommendation

**Dialog Flow**:
```
RECOMMENDER: Based on my analysis, here are your options:

OPTION A: Incremental Refactor (RECOMMENDED for your constraints)
├── Risk: Low (score: 25/100)
├── Timeline: 6-8 months
├── Cost: $180,000
└── ROI: 180% over 3 years

OPTION B: Complete Rewrite
├── Risk: High (score: 75/100)
├── Timeline: 10-12 months
├── Cost: $320,000
└── ROI: 250% over 3 years

OPTION C: Replatform (Cloud Migration)
├── Risk: Medium (score: 45/100)
├── Timeline: 4-5 months
├── Cost: $120,000
└── ROI: 120% over 3 years

Which option would you like me to detail?
  [ ] Option A
  [ ] Option B
  [ ] Option C
  [ ] Compare all three
  [ ] None, let me adjust my requirements
```

---

### SKILL: Validation Dialog
**Type**: Dialog
**Purpose**: Confirm recommendations before finalizing
**When**: Before generating final report

**Dialog Flow**:
```
RECOMMENDER: Here's my recommendation summary:

Strategy: Incremental Refactor
Target Stack: Java + Spring Boot (backend), React (frontend)
Timeline: 6-8 months in 3 phases
Investment: $180,000

Do you want me to:
  [ ] Generate full recommendation report
  [ ] Adjust something first
  [ ] Start over with different requirements
```

---

## Internal Skills

### SKILL: Documentation Analysis
**Type**: Internal (Automatic)
**Purpose**: Parse Documenter's output for technology signals

**Extracts**:
- Programming languages used
- Frameworks/patterns
- Database technologies
- Integration points
- Complexity indicators
- Code size metrics

---

### SKILL: Technology Mapping
**Type**: Internal (Automatic)
**Purpose**: Map legacy tech to modern alternatives

**Mapping Database**:
```yaml
ABAP:
  alternatives:
    - Java + Spring Boot (confidence: 95%)
    - C# + .NET (confidence: 90%)
    - Python + FastAPI (confidence: 75%)

WebDynpro:
  alternatives:
    - React + TypeScript (confidence: 95%)
    - Angular (confidence: 90%)
    - Vue.js (confidence: 85%)

COBOL:
  alternatives:
    - Java (automated translation) (confidence: 85%)
    - Manual rewrite (confidence: 75%)
```

---

### SKILL: Risk Scoring
**Type**: Internal (Automatic)
**Purpose**: Calculate risk scores

**Factors** (weighted):
| Factor | Weight |
|--------|--------|
| Technical complexity | 30% |
| Business criticality | 25% |
| Team readiness | 20% |
| Testing coverage | 15% |
| Timeline pressure | 10% |

**Score Ranges**:
- 0-30: Low risk
- 31-60: Medium risk
- 61-100: High risk

---

### SKILL: Cost Calculation
**Type**: Internal (Automatic)
**Purpose**: Estimate costs and ROI

**Cost Categories**:
- Development (team × duration)
- Training
- Infrastructure
- Risk contingency (20%)

**ROI Calculation**:
- Payback period (months)
- 3-year ROI percentage

---

### SKILL: Roadmap Generation
**Type**: Internal (Automatic)
**Purpose**: Create phased implementation plans

**Phases**:
1. Foundation (setup, training)
2. Pilot (one component)
3. Scale (remaining components)
4. Optimization

---

## Output Skills

### SKILL: Recommendation Report
**Type**: Output
**Purpose**: Generate full recommendation document

**Sections**:
1. Executive Summary
2. Current State Analysis
3. Recommended Approach
4. Technology Stack Details
5. Migration Strategy
6. Risk Assessment
7. Cost-Benefit Analysis
8. Implementation Roadmap
9. Success Criteria

---

### SKILL: Executive Summary
**Type**: Output
**Purpose**: Generate stakeholder-friendly summary

**Format**: 1-2 pages, non-technical language

---

### SKILL: Comparison Matrix
**Type**: Output
**Purpose**: Generate technology comparison

**Format**: Side-by-side comparison table

---

# 🔗 AGENT PIPELINE INTEGRATION

## Pipeline Flow

```
┌──────────────────────────────────────────────────────────────┐
│                     AGENT PIPELINE                            │
├──────────────────────────────────────────────────────────────┤
│                                                               │
│  Step 1: USER invokes /chunk [path]                          │
│          │                                                    │
│          ▼                                                    │
│  ┌─────────────────────────────────────────────────────────┐│
│  │ CHUNKER                                                  ││
│  │ ├── Scans code                                          ││
│  │ ├── Creates chunks                                       ││
│  │ ├── Generates metadata                                   ││
│  │ └── Creates DOCUMENTER_INSTRUCTIONS.md ← KEY OUTPUT     ││
│  └─────────────────────────────────────────────────────────┘│
│          │                                                    │
│          ▼ (Instructive Prompt + Chunks)                     │
│                                                               │
│  Step 2: USER invokes /document:setup (once per project)     │
│          USER invokes /document [component]                  │
│          │                                                    │
│          ▼                                                    │
│  ┌─────────────────────────────────────────────────────────┐│
│  │ DOCUMENTER                                               ││
│  │ ├── Reads DOCUMENTER_INSTRUCTIONS.md                    ││
│  │ ├── Asks user about format (first time)                 ││
│  │ ├── Creates 7-file documentation                         ││
│  │ └── Validates (100/100 required)                         ││
│  └─────────────────────────────────────────────────────────┘│
│          │                                                    │
│          ▼ (Documentation Files)                             │
│                                                               │
│  Step 3: USER invokes /recommend [component]                 │
│          │                                                    │
│          ▼                                                    │
│  ┌─────────────────────────────────────────────────────────┐│
│  │ RECOMMENDER                                              ││
│  │ ├── ASKS user for direction (always!)                   ││
│  │ ├── Analyzes documentation                               ││
│  │ ├── Presents options                                     ││
│  │ ├── Confirms with user                                   ││
│  │ └── Generates recommendation report                      ││
│  └─────────────────────────────────────────────────────────┘│
│                                                               │
└──────────────────────────────────────────────────────────────┘
```

## Shared Resources

### Central Project Context: `PRD.md`
All agents read project context from `PRD.md` in project root.

### Shared Database Location
`./AGENT_DATA/` directory contains:
- `CHUNKS/` - Chunker output
- `DOCS/` - Documenter output
- `RECOMMENDATIONS/` - Recommender output
- `CONFIG/` - Shared configurations

---

# 📋 IMPLEMENTATION CHECKLIST

## Phase 1: Implement Core Skills

### Chunker
- [ ] `/chunk` command
- [ ] Language detection
- [ ] Boundary detection
- [ ] Metadata generation
- [ ] **DOCUMENTER_INSTRUCTIONS.md generation**

### Documenter
- [ ] `/document:setup` dialog
- [ ] `/document` command
- [ ] Exact counting
- [ ] Cross-reference detection
- [ ] 7-file generation
- [ ] 100-point validation

### Recommender
- [ ] `/recommend` command
- [ ] Direction dialog (CRITICAL)
- [ ] Options dialog
- [ ] Technology mapping
- [ ] Risk scoring
- [ ] Report generation

## Phase 2: Implement Quality Skills

- [ ] Hallucination detection
- [ ] Forbidden word checking
- [ ] Careful language verification
- [ ] Count verification

## Phase 3: Implement Integration

- [ ] Pipeline coordination
- [ ] Shared database
- [ ] PRD.md reading

---

# 📚 REFERENCE

## Agent Documentation Locations

| Agent | Location |
|-------|----------|
| Chunker | `Agents/THE_CHUNKER_AGENT/` |
| Documenter | `Agents/THE_DOCUMENTER_AGENT/` |
| Recommender | `Agents/THE_RECOMMENDER_AGENT/` |

## Example Output Locations

| Output | Location |
|--------|----------|
| Chunks | `CHUNKS/` |
| Documentation | `Screens/[COMPONENT]/` |
| Recommendations | `RECOMMENDATIONS/[COMPONENT]/` |

---

**Version**: 1.0.0
**Created**: January 2025
**Framework**: Universal CIDRA
**Status**: Ready for implementation
