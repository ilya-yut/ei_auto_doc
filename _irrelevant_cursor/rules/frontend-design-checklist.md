# Frontend Design Pre-Flight Checklist

**Version:** 1.1
**Created:** 2025-12-26
**Updated:** 2026-01-01
**Purpose:** Mandatory checklist before creating/modifying ANY frontend component

---

## MANDATORY PRE-FLIGHT CHECK

Before writing ANY frontend CSS or component code, VERIFY compliance with these rules.

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                                                                             │
│   STOP! Before writing frontend code, complete this checklist:              │
│                                                                             │
│   [ ] Read .claude/skills/frontend-design/SKILL.md (RULE 17, 18, 19, 20)    │
│   [ ] Read docs/VISUAL_CONSISTENCY_ANALYSIS.md (style specs)                │
│   [ ] Verify NO AI-recognizable patterns will be used                       │
│   [ ] Verify THA default style will be followed                             │
│                                                                             │
│   VIOLATION = REJECTED CODE                                                 │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## RULE 17 COMPLIANCE: No AI-Recognizable Patterns

### BANNED Elements (Do NOT use):

| Pattern | Example | Why Banned |
|---------|---------|------------|
| Colored left bars | `border-left: 4px solid #06b6d4` | AI signature |
| Blue-cyan gradients | `linear-gradient(180deg, #06b6d4, #0891b2)` | AI signature |
| Purple-pink gradients | `linear-gradient(135deg, #8b5cf6, #ec4899)` | AI signature |
| Decorative pseudo-elements | `::before { content: ''; width: 4px; }` | AI signature |
| Glowing effects | `box-shadow: 0 0 20px rgba(6,182,212,0.5)` | AI signature |
| Multiple layered shadows | 3+ box-shadow values for "depth" | AI signature |

### Self-Check Before Committing:

```
[ ] No border-left/right/top/bottom with color for decoration
[ ] No ::before or ::after creating colored bars
[ ] No blue-cyan or purple-pink gradients
[ ] No "glow" or "scan line" effects
[ ] No floating geometric shapes as decoration
```

---

## RULE 18 COMPLIANCE: THA Default Design Style

### MANDATORY Style Rules:

| Element | Correct | Wrong |
|---------|---------|-------|
| Interactive corners | `border-radius: 0` | `border-radius: 6px` |
| Selection indicator | Background color change | Border color change |
| Active state | `background: #f3f4f6` + shadow | `border-left: 3px solid #color` |
| Hover state | `background: #f9fafb` | `border-color: #0a6ed1` |
| Color palette | White + gray (#e5e7eb) | Colored backgrounds |

### Correct Selection State CSS:

```css
/* CORRECT - Use this pattern */
.item {
  background: #ffffff;
  border: 1px solid #e5e7eb;
  border-radius: 0;
}

.item:hover {
  background: #f9fafb;
  box-shadow: 0 4px 12px rgba(0,0,0,0.06);
}

.item.selected {
  background: #f3f4f6;
  box-shadow: 0 4px 16px rgba(0,0,0,0.08);
}
```

### BANNED Selection State CSS:

```css
/* WRONG - Do NOT use */
.item.selected {
  border-left: 3px solid #dc2626;  /* NO colored edge indicators */
  border-color: #0a6ed1;           /* NO border color for selection */
}

.item.severity-critical {
  border-left-color: #dc2626;      /* NO severity via border color */
  background: linear-gradient(...); /* NO gradient backgrounds */
}
```

### Self-Check Before Committing:

```
[ ] All interactive elements have border-radius: 0
[ ] Selection shown via background, NOT border color
[ ] No colored lines on edges (left/right/top/bottom)
[ ] Using white + gray palette primarily
[ ] No gradients for decoration
```

---

## RULE 19 COMPLIANCE: Design Serves Data

### Principles:

1. **Clarity over decoration** - Every visual element aids understanding
2. **Data takes priority** - Numbers/text are most prominent
3. **Clean visual hierarchy** - Eye flows to important info first
4. **No visual noise** - Remove anything that doesn't help comprehension
5. **Efficient scanning** - Key info understood in 2-3 seconds

### Self-Check Before Committing:

```
[ ] Can user understand key data in 2-3 seconds?
[ ] Is design helping or hindering comprehension?
[ ] Would removing any element make data clearer?
[ ] Are numbers/metrics the most prominent elements?
```

---

## RULE 20 COMPLIANCE: Custom Icons Are Sacred

### MANDATORY Rule:

**Custom icons that have been assigned to ANY objects in the system MUST NOT BE ALTERED IN ANY WAY WITHOUT EXPLICIT USER CONSENT.**

This includes:
- Icons assigned to buttons, tabs, badges, or any UI element
- Icons representing specific features, modules, or actions
- SVG icons, icon fonts, or image-based icons
- Icon colors, sizes, or positioning

### Why This Rule Exists:

Custom icons are carefully selected and approved by the user. Once assigned:
- They become part of the product's visual identity
- They have specific meaning to users
- Changing them breaks user expectations
- They may have been iterated on multiple times before approval

### Self-Check Before Committing:

```
[ ] Am I keeping all existing custom icons unchanged?
[ ] Have I received EXPLICIT approval for any icon change?
[ ] Am I NOT replacing approved icons with "standard" alternatives?
[ ] Am I NOT modifying icon colors, sizes, or positions without approval?
```

### If Icon Change Is Needed:

1. **ASK** the user explicitly before making any change
2. **SHOW** the current icon vs proposed change
3. **WAIT** for explicit approval
4. **NEVER** assume "improvement" is wanted

---

## VISUAL CONSISTENCY STANDARDS

### Typography (from docs/VISUAL_CONSISTENCY_ANALYSIS.md):

| Element | Size | Weight | Color |
|---------|------|--------|-------|
| Page Title | 20px | 700 | #dc2626 |
| Section Title | 26px | 600 | #1f2937 |
| Description | 16px | 400 | #4b5563 |
| Button Text | 11px | 700 | varies |

### Font Families:

- **Primary**: `Arial, Helvetica, sans-serif`
- **Buttons/Labels**: `Calibri, 'Segoe UI', Arial, Helvetica, sans-serif`

### Color Palette:

| Purpose | Color |
|---------|-------|
| Primary Red | #dc2626 |
| Background | #ffffff |
| Text Primary | #1f2937 |
| Text Secondary | #6b7280 |
| Border | #e5e7eb |
| Hover Background | #f9fafb |
| Selected Background | #f3f4f6 |

### Spacing:

- Card Border Radius: 0 (interactive) or 12px (static containers)
- Button Border Radius: 4px
- Button Padding: 4px 10px

---

## SEVERITY INDICATION (THA Style)

### CORRECT Way to Show Severity:

Use **text badges** and **subtle background tints**, NOT colored borders:

```css
/* Severity badge - text-based */
.severity-badge {
  font-size: 10px;
  font-weight: 700;
  padding: 2px 6px;
  border-radius: 2px;
}

.severity-badge.critical {
  background: #fef2f2;
  color: #dc2626;
}

.severity-badge.high {
  background: #fff7ed;
  color: #ea580c;
}

.severity-badge.medium {
  background: #fefce8;
  color: #ca8a04;
}

.severity-badge.low {
  background: #f0fdf4;
  color: #16a34a;
}
```

### WRONG Way to Show Severity:

```css
/* DO NOT USE - AI signature patterns */
.card.severity-critical {
  border-left: 4px solid #dc2626;  /* NO */
  background: linear-gradient(90deg, rgba(220,38,38,0.08), transparent);  /* NO */
}
```

---

## ENFORCEMENT

### Before Any Frontend PR/Commit:

1. Run through this checklist
2. Search code for banned patterns:
   - `border-left:` with color values
   - `linear-gradient` for decoration
   - `border-radius:` > 0 on interactive elements
   - `::before` or `::after` creating bars

3. Verify visual result matches THA style

### If Violations Found:

1. DO NOT commit
2. Refactor to compliant patterns
3. Re-verify against checklist

---

## 🔒 FROZEN DESIGN: Alert Discoveries Page (2025-12-31)

**CRITICAL: These specifications are LOCKED. DO NOT MODIFY without explicit user approval.**

### Global Font (ALL Components)

```css
/* MANDATORY - Applied to ALL elements */
* {
  font-family: Calibri, 'Segoe UI', Arial, Helvetica, sans-serif !important;
}
```

### Verdict Strip - Context Row (KPIs)

| Element | Property | Value | LOCKED |
|---------|----------|-------|--------|
| `.verdict-context` | padding | `7px 24px` | 🔒 |
| `.context-value.financial` | color | `#dc2626` | 🔒 |
| `.context-label` | text | "Financial Exposure", "Records Analyzed", "Analysis Period" | 🔒 |

### Verdict Cards (Severity, Fraud Status, Risk Score)

| Element | Property | Value | LOCKED |
|---------|----------|-------|--------|
| `.verdict-card` | padding | `9px 24px !important` | 🔒 |
| `.verdict-card` | background | `#ffffff !important` | 🔒 |
| `.verdict-card` | border | `1px solid #e5e7eb !important` | 🔒 |
| `.verdict-card` | border-radius | `12px !important` | 🔒 |

### Verdict Badge (CRITICAL/HIGH/MEDIUM/LOW, CONFIRMED/INVESTIGATE/MONITOR/NONE)

```css
/* FROZEN - DO NOT MODIFY */
.verdict-badge {
  display: inline-flex !important;
  align-items: center !important;
  gap: 6px !important;
  padding: 6px 12px !important;
  border-radius: 4px !important;
  font-size: 17px !important;      /* LOCKED */
  font-weight: 700 !important;
  letter-spacing: 0.5px !important;
}

.verdict-badge-container {
  min-height: 50px !important;     /* LOCKED */
}
```

### Verdict Badge Colors (THA Compliant)

| Status | Background | Text Color |
|--------|------------|------------|
| CRITICAL | `#fef2f2` | `#dc2626` |
| HIGH | `#fff7ed` | `#ea580c` |
| MEDIUM | `#fefce8` | `#ca8a04` |
| LOW | `#f0fdf4` | `#16a34a` |
| CONFIRMED | `#fef2f2` | `#dc2626` |
| INVESTIGATE | `#fff7ed` | `#ea580c` |
| MONITOR | `#fefce8` | `#ca8a04` |
| NONE | `#f0fdf4` | `#16a34a` |

### Risk Score Display

```css
/* FROZEN - DO NOT MODIFY */
.verdict-score {
  font-size: 33px !important;      /* LOCKED */
  font-weight: 800 !important;
  line-height: 1 !important;
  letter-spacing: -2px !important;
}

.verdict-score-max {
  font-size: 20px;
  font-weight: 500;
  opacity: 0.4;
}
```

### Sidebar Discovery Cards

**PRIMARY FILE:** `frontend/src/components/Layout.css` (lines 199+)
**OVERRIDE FILE:** `frontend/src/pages/AlertDashboard.css` (lines 7982+ for badge specificity)

```css
/* FROZEN - DO NOT MODIFY (Updated 2026-01-01) */
.discovery-cards-container .discovery-card {
  background: #ffffff !important;  /* Pure white default */
  border-radius: 8px !important;   /* LOCKED */
  padding: 16px 24px 20px 24px !important;  /* LOCKED - extra bottom for financial */
  border: 1px solid #e5e7eb !important;
}

.discovery-cards-container .discovery-card:hover {
  background: #f5f5f5 !important;  /* Gray on hover - LOCKED */
}

.discovery-cards-container .discovery-card.selected {
  background: #f5f5f5 !important;  /* Gray when selected - LOCKED */
}
```

### Sidebar Discovery Card Elements

```css
/* FROZEN - DO NOT MODIFY (2026-01-01) */
/* Module badge - sharp corners, neutral background */
.module-badge {
  background: #f5f5f5;
  border-radius: 0 !important;     /* Interactive = sharp corners */
}

/* Severity badge - sharp corners */
.severity-badge {
  border-radius: 0 !important;     /* Interactive = sharp corners */
}

/* Score badge */
.card-score {
  background: #f5f5f5;
  border-radius: 0;                /* Interactive = sharp corners */
  color: #b91c1c;
}

/* Dotted separator */
.card-separator {
  border-top: 1px dotted #d1d5db;
}

/* Financial amount - full format, red text */
.card-financial {
  font-size: 14px;
  font-weight: 700;
  color: #b91c1c;
}
```

### Action Buttons (View Raw, Create Action Item, Analyze & Validate)

```css
/* FROZEN - DO NOT MODIFY (2026-01-01) */
.unified-action-btn {
  background: #f5f5f5;             /* LOCKED - neutral gray */
  border-radius: 6px;
}

.unified-action-btn:hover {
  background: #e8e8e8;             /* LOCKED - slightly darker on hover */
}
```

### Header Row 2 (Purpose Text)

```css
/* FROZEN - Prevents layout jumps */
.discovery-header-row-2 {
  min-height: 77px;               /* LOCKED - Always reserve 3 lines */
  max-height: 77px;               /* LOCKED */
  overflow: hidden;
  padding-top: 7px;               /* LOCKED */
}
```

### DELETED: Discovery1 Page (2025-12-31)

**`frontend/src/pages/discovery1/` has been REMOVED.** It was causing CSS conflicts with AlertDashboard.css.

- Routes removed from `App.tsx`
- All functionality consolidated into `AlertDiscoveries.tsx`
- DO NOT recreate this folder

---

## RELATED DOCUMENTS

- `.claude/skills/frontend-design/SKILL.md` - Full design rules
- `docs/VISUAL_CONSISTENCY_ANALYSIS.md` - Style specifications
- `frontend/src/pages/AlertDashboard.css` - Reference implementation

---

**NO EXCEPTIONS. AI-recognizable patterns = product cannot ship.**

**🔒 FROZEN specifications require EXPLICIT USER APPROVAL to modify.**

---

## CHANGE LOG

| Date | Version | Changes |
|------|---------|---------|
| 2025-12-26 | 1.0 | Initial checklist |
| 2025-12-31 | 1.0 | Added FROZEN specs for verdict strip, header |
| 2026-01-01 | 1.1 | Updated sidebar cards: white default, #f5f5f5 hover/selected, sharp corner badges, dotted separator, score badge. Added action buttons FROZEN spec (#f5f5f5). |
| 2026-01-02 | 1.2 | Added RULE 20: Custom Icons Are Sacred - icons must not be altered without explicit user consent. |
| 2026-01-06 | 1.3 | Added RULE 21: Screenshots Are Evidence - must analyze actual pixels, not assume based on code. |
| 2026-01-06 | 1.4 | Added RULE 22: Positional Consistency - filtered items must stay in original positions, use placeholders for filtered-out items. |

---

## RULE 21 COMPLIANCE: Screenshots Are Evidence, Not Decoration

### MANDATORY Rule:

**When the user sends a screenshot showing a visual problem, ACTUALLY LOOK AT IT. Do not assume what should be there based on code changes. Describe what you ACTUALLY SEE in the image.**

### Why This Rule Exists:

Screenshots are sent specifically to show visual problems. If Claude ignores the visual evidence and claims "looks aligned" or "looks good" when the screenshot clearly shows otherwise, Claude is:
1. Wasting the user's time
2. Ignoring direct evidence
3. Making the user repeat themselves
4. Forcing the user to draw red lines or fix the CSS themselves

### MANDATORY Behavior When Receiving Screenshots:

1. **LOOK** at the actual pixels in the image
2. **DESCRIBE** what you actually see (e.g., "I see two horizontal lines at different heights")
3. **DO NOT** say "looks good" or "looks aligned" unless it actually does
4. **DO NOT** assume the code worked - verify visually
5. **COMPARE** elements that should match (heights, positions, colors)
6. **MEASURE** mentally - are lines at the same Y position? Are colors identical?

### Self-Check When Receiving Screenshots:

```
[ ] Did I actually examine the image, not just glance at it?
[ ] Can I describe specifically what I see (not what I expect)?
[ ] If two things should align, do they ACTUALLY align in the image?
[ ] Am I trusting my eyes over my assumptions about the code?
[ ] Would drawing a straight line across the image show alignment or misalignment?
```

### If You Catch Yourself Saying "Looks Good":

**STOP.** Look again. Describe what you actually see in specific terms:
- "The border line on the left footer is at Y pixels from bottom"
- "The border line on the right footer appears to be X pixels lower/higher"
- "The two lines are NOT aligned - there is a visible gap/offset"

### Consequences of Ignoring Screenshots:

- User loses trust
- Multiple wasted iterations
- User has to fix the problem themselves
- User has to draw on screenshots to make problems obvious
- Conversation becomes frustrating for everyone

---

## RULE 22 COMPLIANCE: Positional Consistency in Filtered Views

### MANDATORY Rule:

**When filtering a list/grid of items, filtered-out items MUST become placeholders in their ORIGINAL positions. Items MUST NOT shift positions when filters change.**

### Why This Rule Exists:

Users build mental maps of where items are located. When filtering changes item positions (e.g., item #3 becomes item #2), users lose context and become confused. This is especially important for:
- Discovery cards
- Alert lists
- Any numbered/positioned items
- Data grids with meaningful order

### CORRECT Implementation:

```typescript
// Filter preserves positions - filtered-out items become null
const filteredItems = items.map(item =>
  matchesFilter(item) ? item : null
);

// Render: null items become placeholders at their original positions
{filteredItems.map((item, idx) =>
  item ? <RealCard data={item} position={idx} /> : <Placeholder position={idx} />
)}
```

**Visual Example:**
```
ALL view:        Filter A:        Filter B:
#1: Item A       #1: Item A       #1: [placeholder]
#2: Item B       #2: [placeholder] #2: Item B
#3: Item C       #3: Item C       #3: [placeholder]
```

### WRONG Implementation:

```typescript
// WRONG - Shifts positions when filtering
const filteredItems = items.filter(item => matchesFilter(item));

// Results in confusing position changes:
// Item C moves from #3 to #2 when Item B is filtered out
```

### Self-Check Before Committing:

```
[ ] Does filtering preserve original item positions?
[ ] Do filtered-out items become placeholders (not removed)?
[ ] Do item numbers (#1, #2, #3) stay consistent across filter changes?
[ ] Can users find the same item at the same position regardless of filter?
```

### Files Currently Using This Pattern:

- `DiscoveryDetailPanel.tsx` - Currency filter for discoveries
- `CriticalDiscoveriesSection.tsx` - Renders filtered discoveries with placeholders

### When Adding New Filters:

1. Use `.map()` with null for filtered items, NOT `.filter()`
2. Render placeholders for null positions
3. Preserve original array length
4. Keep position-based numbering consistent

### RULE 22b: Conditional Filter Visibility

**Filters MUST only be displayed when relevant data exists in the raw data.**

#### Why This Rule Exists:

- **Qualitative alerts (Quali)** have no amounts or quantities - currency filter is irrelevant
- **Many Quantitative alerts** have single currency or no currency data
- Showing empty/irrelevant filters confuses users and wastes screen space

#### MANDATORY Behavior:

| Data Present | Filter Visibility |
|--------------|-------------------|
| Multiple currencies (2+) | Show currency filter |
| Single currency or none | Hide currency filter |
| Multiple categories (2+) | Show category filter |
| Single category or none | Hide category filter |

#### Implementation Pattern:

```typescript
// CORRECT - Only show filter when filtering is meaningful
{sortedCurrencies.length > 1 && (
  <CurrencyFilter ... />
)}

// WRONG - Always showing filter
<CurrencyFilter ... />  // Shows even when only 1 currency exists
```

#### Self-Check Before Adding Filters:

```
[ ] Does filter only appear when 2+ filterable values exist?
[ ] Is filter hidden for Qualitative (Quali) alerts where irrelevant?
[ ] Does filter gracefully handle missing/null data?
[ ] Is there a meaningful reason to filter this data?
```

#### Alert Types Reference:

| Type | Has Amounts | Has Currencies | Filters Applicable |
|------|-------------|----------------|-------------------|
| Quantitative (Quanti) | Yes | Usually | Currency, Amount Range |
| Qualitative (Quali) | No | No | Category, Status only |
