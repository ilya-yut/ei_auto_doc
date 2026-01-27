# Agent Activation Commands
**Version**: 1.0.0
**Framework**: Universal CIDRA

---

## Overview

This document defines the activation commands (slash commands) for invoking each agent in the CIDRA framework. These commands can be used in:
- Claude Code CLI
- Cursor IDE
- VS Code with Claude extension
- Any MCP-compatible environment

---

## THE_CHUNKER_AGENT Commands

### Primary Command
```
/chunk [path]
```
**Description**: Main entry point - chunk code at specified path

**Parameters**:
| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `path` | Yes | - | Directory or file to chunk |
| `--strategy` | No | auto | Override: file_level, function_level, class_level, method_level, semantic |
| `--output` | No | ./CHUNKS/ | Output directory for chunks |
| `--format` | No | json | Output format: json, markdown, both |

**Examples**:
```bash
# Chunk entire source directory
/chunk ./Source\ Code/ABAP/

# Chunk with specific strategy
/chunk ./src --strategy=function_level

# Chunk to custom output directory
/chunk ./legacy --output=./CHUNKS_OUTPUT/ --format=both
```

### Analysis Command
```
/chunk:analyze [path]
```
**Description**: Pre-analysis before chunking - shows what will happen without making changes

**Examples**:
```bash
/chunk:analyze ./Source\ Code/
/chunk:analyze ./legacy/cobol
```

### Status Command
```
/chunk:status
```
**Description**: Show current chunking progress and statistics

---

## THE_DOCUMENTER_AGENT Commands

### Primary Command
```
/document [component]
```
**Description**: Main entry point - document a specific component

**Parameters**:
| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `component` | Yes | - | Component name to document |
| `--template` | No | from setup | Template: enterprise_7_file, compact_5_file, quick_3_file, custom |
| `--language` | No | from setup | Output language: english, hebrew, etc. |

**Examples**:
```bash
# Document a screen using project defaults
/document V_TREATMENT_HISTORY

# Document with template override
/document V_MAIN --template=compact_5_file

# Document in specific language
/document V_SELECT --language=hebrew
```

### Setup Command (Run Once Per Project)
```
/document:setup
```
**Description**: One-time project setup - establish documentation format preferences

**Dialog Flow**:
1. Documentation language (English, Hebrew, bilingual, other)
2. Template selection (7-file, 5-file, 3-file, custom)
3. Output directory preference
4. Validation report naming convention

**Example**:
```bash
/document:setup
# Responds with interactive dialog to configure project preferences
```

### Validation Command
```
/document:validate [path]
```
**Description**: Run 100-point validation on existing documentation

**Examples**:
```bash
/document:validate ./Screens/V_MAIN_SCREEN/
/document:validate ./docs/component_a/
```

### Auto-Fix Command
```
/document:fix [path]
```
**Description**: Auto-fix common documentation issues

**Examples**:
```bash
/document:fix ./Screens/V_OLD_MAIN_SCREEN/
```

---

## THE_RECOMMENDER_AGENT Commands

### Primary Command
```
/recommend [component]
```
**Description**: Start recommendation dialog for a component

**Parameters**:
| Parameter | Required | Default | Description |
|-----------|----------|---------|-------------|
| `component` | Yes | - | Component to analyze (should have documentation) |
| `--quick` | No | false | Skip detailed dialog, use defaults |

**Examples**:
```bash
# Full interactive recommendation (recommended)
/recommend V_MAIN

# Quick recommendation without dialog
/recommend V_SELECT --quick
```

**Dialog Flow** (when not using --quick):
1. What type of recommendations? (tech alternatives, migration, risk, cost, all)
2. What are your constraints? (budget, timeline, team size)
3. Team's current skill set?
4. Risk tolerance level?

### Technology Comparison Command
```
/recommend:compare [tech1] [tech2]
```
**Description**: Compare two technologies side-by-side

**Examples**:
```bash
/recommend:compare ABAP Java
/recommend:compare WebDynpro React
/recommend:compare COBOL Python
```

### Risk Assessment Command
```
/recommend:risk [component]
```
**Description**: Run risk assessment without full recommendation

**Examples**:
```bash
/recommend:risk V_MAIN
/recommend:risk legacy_billing_module
```

---

## Pipeline Commands (Cross-Agent)

### Full Pipeline
```
/pipeline:full [path]
```
**Description**: Run complete Chunker -> Documenter -> Recommender pipeline

**Example**:
```bash
/pipeline:full ./Source\ Code/ABAP/
```

### Chunk and Document
```
/pipeline:chunk-doc [path]
```
**Description**: Run Chunker then Documenter

### Document and Recommend
```
/pipeline:doc-rec [component]
```
**Description**: Run Documenter then Recommender

---

## Quick Reference Card

| Agent | Main Command | Common Options |
|-------|--------------|----------------|
| **Chunker** | `/chunk [path]` | `--strategy`, `--format`, `--output` |
| **Documenter** | `/document [component]` | `--template`, `--language` |
| **Recommender** | `/recommend [component]` | `--quick` |

| Utility Command | Agent | Purpose |
|-----------------|-------|---------|
| `/chunk:analyze` | Chunker | Preview chunking plan |
| `/chunk:status` | Chunker | Check progress |
| `/document:setup` | Documenter | One-time project config |
| `/document:validate` | Documenter | Quality check (100-point) |
| `/document:fix` | Documenter | Auto-repair docs |
| `/recommend:compare` | Recommender | Tech comparison |
| `/recommend:risk` | Recommender | Risk assessment only |

---

## Environment Variables

```bash
# Set default output directory
export CIDRA_OUTPUT_DIR="./output"

# Set default documentation language
export CIDRA_DOC_LANGUAGE="english"

# Set default template
export CIDRA_TEMPLATE="enterprise_7_file"

# Enable verbose logging
export CIDRA_VERBOSE="true"
```

---

## Configuration Files

### Project Configuration
Create `cidra.config.yaml` in project root:

```yaml
project:
  name: "My Legacy System"

chunker:
  default_strategy: "auto"
  output_directory: "./CHUNKS/"

documenter:
  template: "enterprise_7_file"
  language: "english"
  output_directory: "./Screens/"

recommender:
  default_risk_tolerance: "medium"
  include_cost_analysis: true
```

---

## Error Handling

### Common Errors and Solutions

| Error | Cause | Solution |
|-------|-------|----------|
| "No files found" | Path invalid or empty | Verify path exists and contains code files |
| "Setup required" | `/document:setup` not run | Run `/document:setup` first |
| "No documentation found" | Component not documented | Run `/document` before `/recommend` |
| "Validation failed (X/100)" | Quality issues detected | Run `/document:fix` or review manually |

---

## Best Practices

1. **Always run `/document:setup` first** - Configure project preferences once
2. **Use `/chunk:analyze` before chunking** - Preview what will happen
3. **Run `/document:validate` after documenting** - Ensure 100/100 quality
4. **Don't skip `/recommend` dialog** - Better recommendations with context
5. **Check `/chunk:status` for large projects** - Monitor long-running operations

---

*Document Version 1.0.0*
*CIDRA Universal Framework*
