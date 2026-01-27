# Discoveries Page UI/UX Redesign Plan

**Created:** 2025-12-26
**Status:** In Progress
**Revert Point:** `git reset --hard 34c3b55`

---

## Executive Summary

Transform the current text-heavy, cramped Discoveries page into a scannable, visually-driven interface that surfaces actionable insights immediately.

---

## Current Problems

| # | Problem | Severity |
|---|---------|----------|
| 1 | No Visual Hierarchy | HIGH |
| 2 | Text-Heavy Presentation | HIGH |
| 3 | Cramped Layout | MEDIUM |
| 4 | Flat Alert Sidebar | MEDIUM |
| 5 | Buried Concentration Data | HIGH |
| 6 | Inconsistent Colors | LOW |
| 7 | Generic Aesthetics | LOW |

---

## Solution: Three-Zone Layout

```
┌──────────────────────────────────────────────────────────────────────────┐
│ HEADER BAR (existing)                                                    │
├────────────┬─────────────────────────────────────────────────────────────┤
│            │  HERO ZONE - Concentration Chart + Metric Cards             │
│  SIDEBAR   ├─────────────────────────────────────────────────────────────┤
│  (cards)   │  STORY ZONE - Discovery Cards + Key Findings Table          │
│            ├─────────────────────────────────────────────────────────────┤
│            │  DETAILS ZONE - Collapsible: Actions, Technical, Raw Data   │
└────────────┴─────────────────────────────────────────────────────────────┘
```

---

## Implementation Phases

### Phase 1: Sidebar Cards
- [ ] Create `AlertSidebarCard.tsx` with severity stripes
- [ ] Add sidebar CSS styles
- [ ] Integrate into AlertDiscoveries.tsx

### Phase 2: Hero Zone
- [ ] Create `ConcentrationHero.tsx` with donut chart
- [ ] Create `MetricCard.tsx` for KPIs
- [ ] Add CSS variables for severity/module colors

### Phase 3: Discovery Cards
- [ ] Create `DiscoveryCard.tsx`
- [ ] Modify DiscoveryDetailPanel for card grid

### Phase 4: Collapsible Sections
- [ ] Create `CollapsibleSection.tsx`
- [ ] Reorganize details into collapsible sections

### Phase 5: Polish
- [ ] Spacing audit
- [ ] Animation polish
- [ ] Integration testing

---

## Files to Create

| File | Purpose |
|------|---------|
| `features/sidebar/AlertSidebarCard.tsx` | Alert card with severity stripe |
| `features/concentration-hero/ConcentrationHero.tsx` | Donut chart + metrics |
| `features/concentration-hero/MetricCard.tsx` | Reusable metric display |
| `features/detail-panel/DiscoveryCard.tsx` | Visual discovery card |
| `components/CollapsibleSection.tsx` | Expandable section wrapper |

## Files to Modify

| File | Changes |
|------|---------|
| `AlertDiscoveries.tsx` | Add sidebar, restructure layout |
| `DiscoveryDetailPanel.tsx` | Replace bullets with cards |
| `AlertDashboard.css` | Add ~400 lines for new components |
| `dashboard.css` | Add CSS variables |

---

## CSS Design Tokens

```css
/* Severity Colors */
--severity-critical: #dc2626;
--severity-high: #f97316;
--severity-medium: #eab308;
--severity-low: #22c55e;

/* Module Colors */
--module-fi: #3b82f6;
--module-mm: #06b6d4;
--module-sd: #8b5cf6;
--module-md: #14b8a6;
```

---

## Success Criteria

1. **3-Second Test**: Critical issue identifiable within 3 seconds
2. **Above the Fold**: Key insights visible without scrolling
3. **Visual Differentiation**: CRITICAL alerts distinguishable from LOW
4. **Scannable**: Entire page skimmable in under 10 seconds

---

*Plan approved. Implementation in progress.*
