---
name: frontend-design
description: Create distinctive, production-grade frontend interfaces with high design quality. Use this skill when the user asks to build web components, pages, or applications. Generates creative, polished code that avoids generic AI aesthetics.
license: Complete terms in LICENSE.txt
---

This skill guides creation of distinctive, production-grade frontend interfaces that avoid generic "AI slop" aesthetics. Implement real working code with exceptional attention to aesthetic details and creative choices.

The user provides frontend requirements: a component, page, application, or interface to build. They may include context about the purpose, audience, or technical constraints.

---

## MANDATORY PRE-FLIGHT CHECK (DO THIS FIRST)

```text
┌─────────────────────────────────────────────────────────────────────────────┐
│                                                                             │
│   BEFORE WRITING ANY FRONTEND CODE, YOU MUST:                               │
│                                                                             │
│   1. READ .claude/rules/frontend-design-checklist.md                        │
│   2. READ docs/VISUAL_CONSISTENCY_ANALYSIS.md                               │
│   3. VERIFY your design will NOT use AI-recognizable patterns               │
│   4. CONFIRM compliance with THA default style (RULE 18)                    │
│                                                                             │
│   QUICK REMINDERS:                                                          │
│   • NO border-left/right colored bars for selection                         │
│   • NO blue-cyan or purple-pink gradients                                   │
│   • border-radius: 0 on all interactive elements                            │
│   • Selection = background color shift + shadow (NOT border color)          │
│   • Font: Arial, Helvetica, sans-serif                                      │
│   • Primary red: #dc2626 | Border: #e5e7eb | Text: #1f2937                  │
│                                                                             │
│   SKIP THIS = REJECTED CODE                                                 │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

**Reference:** `.claude/rules/frontend-design-checklist.md` for full checklist

---

## CRITICAL: Screenshot Examination Protocol

**BEFORE making any frontend changes:**

1. **If user attaches screenshots:**
   - EXAMINE them visually (don't just read the file)
   - Identify EXACTLY what's wrong visually
   - Take notes of specific issues (sizes, colors, positions, text rendering)
   - Compare to what should be there

2. **Use browser devtools to inspect:**
   - Check computed styles (not just CSS file)
   - Verify actual rendered dimensions
   - Check for conflicting CSS rules
   - Test changes in browser console first

3. **Iteration workflow:**
   - Docker rebuild is SLOW (2-3 min per iteration)
   - Consider suggesting local dev server for CSS work
   - Always verify changes in browser before claiming success
   - Take screenshot after changes to compare

4. **CSS Debugging Checklist:**
   - [ ] Examined user's screenshot
   - [ ] Identified specific visual issues
   - [ ] Checked computed styles in browser
   - [ ] Made changes based on actual rendered state
   - [ ] Verified in browser (not just code)
   - [ ] Compared result to user's requirements

**NEVER claim "it's fixed" without examining the visual result.**

---

## Design Thinking

Before coding, understand the context and commit to a BOLD aesthetic direction:

- **Purpose**: What problem does this interface solve? Who uses it?
- **Tone**: Pick an extreme: brutally minimal, maximalist chaos, retro-futuristic, organic/natural, luxury/refined, playful/toy-like, editorial/magazine, brutalist/raw, art deco/geometric, soft/pastel, industrial/utilitarian, etc. There are so many flavors to choose from. Use these for inspiration but design one that is true to the aesthetic direction.
- **Constraints**: Technical requirements (framework, performance, accessibility).
- **Differentiation**: What makes this UNFORGETTABLE? What's the one thing someone will remember?

**CRITICAL**: Choose a clear conceptual direction and execute it with precision. Bold maximalism and refined minimalism both work - the key is intentionality, not intensity.

Then implement working code (HTML/CSS/JS, React, Vue, etc.) that is:

- Production-grade and functional
- Visually striking and memorable
- Cohesive with a clear aesthetic point-of-view
- Meticulously refined in every detail

## Frontend Aesthetics Guidelines

Focus on:

- **Typography**: Choose fonts that are beautiful, unique, and interesting. Avoid generic fonts like Arial and Inter; opt instead for distinctive choices that elevate the frontend's aesthetics; unexpected, characterful font choices. Pair a distinctive display font with a refined body font.
- **Color & Theme**: Commit to a cohesive aesthetic. Use CSS variables for consistency. Dominant colors with sharp accents outperform timid, evenly-distributed palettes.
- **Motion**: Use animations for effects and micro-interactions. Prioritize CSS-only solutions for HTML. Use Motion library for React when available. Focus on high-impact moments: one well-orchestrated page load with staggered reveals (animation-delay) creates more delight than scattered micro-interactions. Use scroll-triggering and hover states that surprise.
- **Spatial Composition**: Unexpected layouts. Asymmetry. Overlap. Diagonal flow. Grid-breaking elements. Generous negative space OR controlled density.
- **Backgrounds & Visual Details**: Create atmosphere and depth rather than defaulting to solid colors. Add contextual effects and textures that match the overall aesthetic. Apply creative forms like gradient meshes, noise textures, geometric patterns, layered transparencies, dramatic shadows, decorative borders, custom cursors, and grain overlays.

NEVER use generic AI-generated aesthetics like overused font families (Inter, Roboto, Arial, system fonts), cliched color schemes (particularly purple gradients on white backgrounds), predictable layouts and component patterns, and cookie-cutter design that lacks context-specific character.

---

## RULE 17: NO AI-RECOGNIZABLE DESIGN PATTERNS (CRITICAL)

**These patterns are BANNED - they instantly mark a product as "AI-generated":**

### 1. Colored Vertical Bars on Left Side of Elements

- ❌ Blue/cyan gradient bars next to titles or headings
- ❌ Colored accent lines on left side of cards, buttons, or panels
- ❌ `::before` pseudo-elements creating decorative left borders
- ❌ Any vertical colored strip as a "modern" accent

**Example of BANNED code:**

```css
/* DO NOT DO THIS */
.title::before {
  content: '';
  display: inline-block;
  width: 4px;
  height: 24px;
  background: linear-gradient(180deg, #06b6d4 0%, #0891b2 100%);
  border-radius: 2px;
}
```

### 2. Generic AI Gradient Combinations

- ❌ Blue-to-cyan gradients (`#06b6d4 → #0891b2`) as decorative accents
- ❌ Purple-to-pink gradients as "modern" touches
- ❌ Teal-to-blue gradients on buttons or highlights
- These are the DEFAULT color choices of every AI system

### 3. Unnecessary Decorative Elements

- ❌ Curved/rounded accent shapes without functional purpose
- ❌ Glowing effects, scan lines, "futuristic" styling
- ❌ Multiple layered shadows for fake "depth"
- ❌ Floating geometric shapes as background decoration

### WHY THIS RULE EXISTS

User statement (2025-12-26):
> "DO NOT USE THE DEFAULT AI DESIGNS LIKE ASSIGNING A COLORED BLUE CURVED LINE ON THE LEFT OF THE OBJECT. This shit is widely used by agentic systems, thus depriving me from the right to issue a product which will not be 'agentic recognizable' from the 1st glance."

### CORRECT ALTERNATIVES

Instead of decorative bars, use:

- **Font weight and size differences** for visual hierarchy
- **Subtle background color changes** for section differentiation
- **Border thickness variations** (not colored accent bars)
- **White space and padding** for breathing room
- **Typography choices** that create distinction

**When in doubt: LESS decoration, not more.**

**VIOLATION = Product looks "AI-generated" and cannot be shipped.**

---

## RULE 18: THA DEFAULT DESIGN STYLE (MANDATORY)

**This is the approved design language for Treasure Hunt Analyzer:**

### Core Principles

1. **Sharp corners only** - `border-radius: 0` on all interactive elements (cards, buttons, inputs, panels)
2. **No colored edge indicators** - Never use left/right/top/bottom colored lines to show selection
3. **Background-based selection** - Show active/selected state via subtle background color shift + shadow depth
4. **Typography-driven hierarchy** - Use font weight, size, and color for visual distinction
5. **Minimal decoration** - No gradients, no glows, no decorative pseudo-elements
6. **White + gray palette** - White backgrounds, gray borders (#e5e7eb), subtle gray for hover/selected (#f3f4f6, #f9fafb)

### Selection/Active States

```css
/* CORRECT approach */
.item { background: #ffffff; border: 1px solid #e5e7eb; }
.item:hover { background: #f9fafb; box-shadow: 0 4px 12px rgba(0,0,0,0.06); }
.item.selected { background: #f3f4f6; box-shadow: 0 4px 16px rgba(0,0,0,0.08); }
```

### What NOT to do

- ❌ `border-radius: 6px` or any rounded corners
- ❌ `border-left: 3px solid #color` for selection
- ❌ `border-color: #0a6ed1` to indicate active state
- ❌ Colored lines/bars anywhere on card edges
- ❌ Multiple visual indicators for same state (background AND border AND shadow)

**This style was approved on 2025-12-26 and MUST be used consistently across the entire application.**

---

## RULE 19: DESIGN AS A GATEWAY TO DATA (CRITICAL)

**Design exists to SERVE information comprehension, not to compete with it.**

### Core Philosophy

The design MUST NOT overlap or obstruct the quick and easy perception of data. At the end, design is just:

- A **gateway**
- A **path**
- A **journey** for the user to comprehend the information they look at

### Design Requirements

1. **Clarity over decoration** - Every visual element must aid understanding, not distract from it
2. **Data takes priority** - Numbers, text, and metrics must be the most prominent elements
3. **Clean visual hierarchy** - User's eye should naturally flow to the most important information first
4. **No visual noise** - Remove anything that doesn't directly help comprehension
5. **Efficient scanning** - User should understand key information within 2-3 seconds of looking

### What This Means in Practice

```
✅ CORRECT: Clean card with prominent $50.0M value, clear labels, instant comprehension
❌ WRONG: Decorative card with gradients, borders, badges competing for attention
```

### The Test

Before finalizing any design, ask:

- Can the user understand the key data in 2-3 seconds?
- Is the design helping or hindering comprehension?
- Would removing any element make the data clearer?

**If design competes with data for attention → DESIGN FAILS**

**Design is the servant of information, not its master.**

---

Interpret creatively and make unexpected choices that feel genuinely designed for the context. No design should be the same. Vary between light and dark themes, different fonts, different aesthetics. NEVER converge on common choices (Space Grotesk, for example) across generations.

**IMPORTANT**: Match implementation complexity to the aesthetic vision. Maximalist designs need elaborate code with extensive animations and effects. Minimalist or refined designs need restraint, precision, and careful attention to spacing, typography, and subtle details. Elegance comes from executing the vision well.

Remember: Claude is capable of extraordinary creative work. Don't hold back, show what can truly be created when thinking outside the box and committing fully to a distinctive vision.
