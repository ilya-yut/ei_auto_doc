# THA Frontend Pre-Task Protocol

**Read this BEFORE any design/frontend task.**

---

## 1. READ THESE DOCUMENTS IN ORDER (mandatory):

```
.claude/rules/document-authority.md      → Which docs are authoritative
.claude/rules/frontend-design-checklist.md → Design rules + FROZEN specs
llm_handover.md                          → Current project state
CLAUDE.md                                → Project structure
```

---

## 2. THA DESIGN RULES - MEMORIZE THESE:

### ❌ BANNED (AI-signature patterns):

**COLOR SCHEMES:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| Blue-cyan palette | `#06b6d4`, `#0891b2`, `#22d3ee` | AI signature |
| Purple-violet palette | `#8b5cf6`, `#a855f7`, `#7c3aed` | AI signature |
| Pink-magenta palette | `#ec4899`, `#f472b6`, `#db2777` | AI signature |
| Teal accent colors | `#14b8a6`, `#2dd4bf` | AI signature |
| Indigo accent colors | `#6366f1`, `#4f46e5` | AI signature |
| Neon/vibrant accents | Any oversaturated "modern" colors | AI signature |

**GRADIENTS:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| Blue-to-cyan gradient | `linear-gradient(#3b82f6, #06b6d4)` | AI signature |
| Purple-to-pink gradient | `linear-gradient(#8b5cf6, #ec4899)` | AI signature |
| Purple-to-blue gradient | `linear-gradient(#8b5cf6, #3b82f6)` | AI signature |
| Green-to-teal gradient | `linear-gradient(#10b981, #14b8a6)` | AI signature |
| Orange-to-yellow gradient | `linear-gradient(#f97316, #eab308)` | AI signature |
| Diagonal gradients | `linear-gradient(135deg, ...)` | AI signature |
| Radial gradients | `radial-gradient(...)` | AI signature |
| Gradient text | `background-clip: text` | AI signature |
| Mesh gradients | Multiple color stop gradients | AI signature |

**BORDERS & INDICATORS:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| Colored left/right bars | `border-left: 4px solid #color` | AI signature |
| Pill-shaped elements | `border-radius: 9999px` or `50%` | AI signature |
| Rounded tabs | `border-radius: 8px 8px 0 0` | AI signature |
| Colored focus rings | `outline: 2px solid #3b82f6` | AI signature |
| Ring utility | `ring-2 ring-blue-500` (Tailwind) | AI signature |
| Animated borders | `border-color` transitions | AI signature |
| Double borders | `border` + `outline` combo | AI signature |
| Dashed accent borders | `border: 2px dashed #color` | AI signature |

**SHADOWS & GLOW:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| Glow effects | `box-shadow: 0 0 20px rgba(...)` | AI signature |
| Colored shadows | `box-shadow: 0 4px 14px rgba(59,130,246,0.4)` | AI signature |
| Multi-layer shadows | 3+ shadow values stacked | AI signature |
| Large soft shadows | `box-shadow: 0 25px 50px -12px` | AI signature |
| Inner glow | `box-shadow: inset 0 0 20px` | AI signature |
| Text glow | `text-shadow: 0 0 10px` | AI signature |
| Neon glow on dark | `0 0 5px, 0 0 10px, 0 0 20px` stacked | AI signature |

**GLASSMORPHISM / BLUR:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| Frosted glass | `backdrop-filter: blur(10px)` | AI signature |
| Translucent backgrounds | `rgba(255,255,255,0.7)` + blur | AI signature |
| Blurred modals | Modal with backdrop blur | AI signature |
| Sticky header blur | Header with `backdrop-filter` | AI signature |

**PSEUDO-ELEMENTS:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| ::before/::after bars | Colored decorative lines | AI signature |
| Animated underlines | `::after` expanding on hover | AI signature |
| Decorative shapes | `::before` with `border-radius: 50%` | AI signature |
| Gradient overlays | `::after` with gradient | AI signature |

**HOVER/INTERACTIVE STATES:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| Scale on hover | `transform: scale(1.05)` | AI signature |
| Lift effect | `translateY(-2px)` + shadow increase | AI signature |
| Color shift to accent | `hover:bg-blue-500` | AI signature |
| Underline slide-in | Animated border-bottom | AI signature |
| Ripple effect | Material-style click ripple | AI signature |
| Brightness change | `filter: brightness(1.1)` | AI signature |
| Border color change | `hover:border-blue-500` | AI signature |

**ANIMATIONS:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| Fade-in on load | `opacity: 0 → 1` animation | AI signature |
| Slide-in from side | `translateX(-20px) → 0` | AI signature |
| Bounce animation | `@keyframes bounce` | AI signature |
| Pulse for attention | `@keyframes pulse` | AI signature |
| Skeleton shimmer | Gradient animation left-to-right | AI signature |
| Excessive transitions | `transition: all 0.3s ease` everywhere | AI signature |

**TYPOGRAPHY:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| Inter font | Default AI font choice | AI signature |
| Poppins font | Second most common AI font | AI signature |
| Gradient text | Colored text via background-clip | AI signature |
| Oversized hero headings | `font-size: 48px+` | AI signature |
| Excessive letter-spacing | `letter-spacing: 0.1em` on headings | AI signature |
| Text shadows | `text-shadow: 2px 2px 4px` | AI signature |

**DECORATIVE ELEMENTS:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| SVG blob shapes | Organic curved backgrounds | AI signature |
| Floating circles | Decorative `border-radius: 50%` shapes | AI signature |
| Geometric decorations | Triangles, hexagons as decoration | AI signature |
| Dot patterns | Grid of small circles | AI signature |
| Abstract illustrations | Colorful vector art | AI signature |
| Emoji as icons | 🚀 ✨ 💡 in UI elements | AI signature |

**COMPONENT PATTERNS:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| Pill badges | `border-radius: 9999px` badges | AI signature |
| Animated toggles | Smooth sliding toggle switches | AI signature |
| Progress bar gradient | Gradient fill in progress bars | AI signature |
| Tooltip with arrow | Popover with CSS triangle | AI signature |
| Icon in colored circle | Icon with `bg-blue-100` circle behind | AI signature |
| Cards with hover lift | Card rises + shadow on hover | AI signature |

**DARK MODE SIGNATURES:**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| Neon accents on dark | Bright colors on `#0f172a` | AI signature |
| Glowing buttons | Button with shadow glow | AI signature |
| Gradient borders on dark | `border-image: linear-gradient` | AI signature |
| Blue/purple dark theme | `#1e1b4b` backgrounds | AI signature |

**ICONS (CRITICAL - DO NOT TOUCH):**
| Pattern | Example | Why Banned |
|---------|---------|------------|
| Removing existing icons | Deleting icons from buttons/UI | **STRICTLY FORBIDDEN** |
| Changing approved icons | Remove/replace custom icons | AI overreach |
| Heroicons everywhere | Default AI icon library | AI signature |
| Lucide/Feather defaults | Common AI icon choices | AI signature |
| Icons from other sources | Any icon not from project folder | Inconsistent style |

### ✅ REQUIRED (THA style):

| Element | Correct | Wrong |
|---------|---------|-------|
| Selection indicator | Background color change | Border color change |
| **Buttons/Interactive** | `border-radius: 4px` | Sharp corners |
| Static containers | `border-radius: 0` | Rounded corners |
| **Badges/Tags** | `border-radius: 0` (sharp) | Pill/rounded (`border-radius: 9999px`) |
| Severity display | Text badge + tinted bg | Colored border |
| Font | Calibri, 'Segoe UI', Arial | Any other |
| Palette | White + gray (#e5e7eb) | Colored backgrounds |
| Section headings | Sentence case ("Concentration Analysis") | ALL CAPS |
| **Header names** | Capital only on 1st letter ("Medium") | ALL CAPS ("MEDIUM") |
| **Analysis section text** | `font-size: 16px` | Any other size |
| **Page titles** | Skywind red underline (`border-bottom: 0.25px solid #e31e24`) | No underline or other colors |

### 🎯 ICON RULES (MANDATORY):

1. **NEVER REMOVE ICONS** - If a button/element has an icon, DO NOT remove it
2. **ICON SOURCE**: Use ONLY icons from this folder:
   ```
   frontend/src/components/icons/
   ```
3. **Adding new icons**: If you need an icon that doesn't exist, ASK the user first
4. **Existing icons are sacred**: Treat all existing icons as approved and unchangeable

### 🔄 CONSISTENCY RULES (CRITICAL):

**Rule 1: Same Data = Same Display Everywhere**
- If a badge shows "Business Control" on cards, headers MUST also show "Business Control"
- If sidebar shows "Access Governance", detail panel MUST show "Access Governance"
- NEVER mix different representations of the same concept across views

**Rule 2: Use Full Descriptions, Not Abbreviations**
| Correct | Wrong | Why |
|---------|-------|-----|
| Business Control | BC | Users don't know abbreviations |
| Access Governance | AG | Internal codes confuse users |
| Business Protection | BP | Not obvious what it means |
| Technical Control | TC | Abbreviation unclear |
| Materials Management | MM | SAP-specific jargon |

**Rule 3: Case Rules for Labels**
| Element | Correct | Wrong |
|---------|---------|-------|
| Categories/Modules | "Business Control" (Title Case) | "BUSINESS_CONTROL" or "BC" |
| Badges | "Business Control" | "BUSINESS CONTROL" |
| Section headings | "Concentration Analysis" (Sentence case) | "CONCENTRATION ANALYSIS" |

**Rule 4: Before Displaying Any Category/Module**
1. Check how it's displayed elsewhere (cards, sidebar, headers)
2. Use the EXACT same format
3. If uncertain, use full human-readable description in Title Case

**Rule 5: Number Formatting (ALL pages EXCEPT Discoveries)**
| Type | Format | Example |
|------|--------|---------|
| Amounts/Currency | 1,000 separator, NO decimals | `$1,234,567` not `$1,234,567.89` |
| Percentages | 2 decimal places | `45.67%` not `45.7%` or `46%` |
| File sizes | 2 decimal places (MB) | `1.23 MB` |
| Time durations | 1 decimal place (seconds) | `2.5s` |

**Note:** The Discoveries entry (AlertDashboard/AlertDiscoveries) is EXEMPT from these formatting rules.

---

## 3. 🔒 FROZEN SPECIFICATIONS (DO NOT MODIFY WITHOUT USER APPROVAL):

### Verdict Strip:
```css
.verdict-context { padding: 7px 24px; }
.verdict-card { padding: 9px 24px; border-radius: 12px; }
.verdict-badge { font-size: 17px; padding: 6px 12px; }
.verdict-badge-container { min-height: 50px; }
.verdict-score { font-size: 33px; }
```

### Sidebar Discovery Cards (Updated 2026-01-01):
**File: `frontend/src/components/Layout.css`** (lines 199+)
```css
.discovery-cards-container .discovery-card {
  background: #ffffff;           /* Pure white default */
  padding: 16px 24px 20px 24px;  /* Extra bottom for financial amount */
  border-radius: 8px;
}
.discovery-card:hover { background: #f5f5f5; }
.discovery-card.selected { background: #f5f5f5; }

/* Card elements - sharp corners */
.module-badge { background: #f5f5f5; border-radius: 0; }
.severity-badge { border-radius: 0; }
.card-score { background: #f5f5f5; border-radius: 0; color: #b91c1c; }
.card-separator { border-top: 1px dotted #d1d5db; }
.card-financial { font-size: 14px; color: #b91c1c; }
```
**Overrides (at end of AlertDashboard.css, lines 7982+):**
Badge border-radius: 0 rules with high specificity

### Action Buttons (2026-01-01):
```css
.unified-action-btn { background: #f5f5f5; }
.unified-action-btn:hover { background: #e8e8e8; }
```

### Header:
```css
.discovery-header-row-2 {
  min-height: 77px;
  max-height: 77px;
  padding-top: 7px;
}
```

### Global Font:
```css
* { font-family: Calibri, 'Segoe UI', Arial, Helvetica, sans-serif !important; }
```

---

## 4. CSS FILE LOCATIONS:

### Primary CSS Files (modify these):
| File | Purpose |
|------|---------|
| `frontend/src/pages/AlertDashboard.css` | Main dashboard styles, verdict cards, FROZEN overrides at end |
| `frontend/src/components/Layout.css` | **Sidebar discovery cards**, navigation |
| `frontend/src/index.css` | Global fonts |

### Secondary CSS Files (feature-specific):
| File | Purpose |
|------|---------|
| `frontend/src/pages/alert-discoveries/features/detail-panel/DiscoveryDetailPanel.css` | Detail panel styles |
| `frontend/src/pages/alert-discoveries/features/section-actions/SectionModals.css` | View Raw/Info modals |
| `frontend/src/pages/alert-discoveries/features/alert-summary/AlertSummary.css` | Alert summary component |
| `frontend/src/pages/alert-discoveries/features/create-action-item/CreateActionItemModal.css` | Action item modal |
| `frontend/src/pages/alert-discoveries/features/json-popovers/JsonDataPopover.css` | JSON popovers |
| `frontend/src/pages/alert-discoveries/features/analysis-log/AnalysisLogPopover.css` | Analysis log |

### Page-specific CSS:
| File | Purpose |
|------|---------|
| `frontend/src/pages/Upload.css` | Upload page |
| `frontend/src/pages/AlertAnalysis.css` | Alert analysis page |
| `frontend/src/pages/Findings.css` | Findings page |
| `frontend/src/pages/Reports.css` | Reports page |
| `frontend/src/pages/Logs.css` | Logs page |
| `frontend/src/pages/Maintenance.css` | Maintenance page |

### Component CSS:
| File | Purpose |
|------|---------|
| `frontend/src/components/KpiCard.css` | KPI cards |
| `frontend/src/components/SkywindLogo.css` | Logo styling |
| `frontend/src/components/AnalysisProgress.css` | Progress indicator |
| `frontend/src/styles/dashboard.css` | Dashboard (legacy?) |

**DELETED:** `frontend/src/pages/discovery1/` - Do NOT recreate

**⚠️ CSS CONFLICT PREVENTION:**
- Sidebar discovery card styles: ONLY in `Layout.css`
- Badge overrides: At END of `AlertDashboard.css` (lines 7982+)
- Do NOT duplicate `.discovery-card` rules across files

---

## 5. PRE-IMPLEMENTATION CHECKLIST:

```
□ Have I read the file I'm about to modify?
□ Am I using !important where needed for specificity?
□ Am I avoiding ALL banned patterns?
□ Does my change comply with THA design rules?
□ Am I NOT modifying FROZEN specs without approval?
□ Will I test on correct port (localhost:3000)?
```

---

## 6. POST-IMPLEMENTATION CHECKLIST:

```
□ Search code for: border-left:, linear-gradient, ::before bars
□ Verify no AI-signature patterns
□ Confirm THA style compliance
□ Test visually on correct port
```

---

## 7. IF UNCERTAIN:

- **ASK** before implementing
- **SHOW** THA-compliant alternatives
- **NEVER** guess or use "typical" design patterns

---

## ACKNOWLEDGMENT REQUIRED:

Before proceeding with any frontend task, state:
1. "I have read the frontend pre-task protocol"
2. List any FROZEN values the task might affect
3. Confirm no banned patterns will be used

---

*Last Updated: 2026-01-02 (Added NUMBER FORMATTING rules - Rule 5: no decimals for amounts, 2 decimals for percentages, Discoveries exempt)*
