## General Overview

This Exception Indicator reads RFC destination master data together with attribute timestamps from the join of destination and attribute tables, then enriches each row with a computed duration from last change to the evaluation clock so operations can spot destinations that have been stable or unusually idle for too long.

This EI helps by:
- Giving Basis and integration teams a filtered list of destinations with creator, changer, and change-date context without exporting full tables manually
- Supporting reviews after security or landscape changes when attribute change patterns should be reconciled quickly
- Highlighting destinations whose elapsed time since last modification falls outside configured duration bands after date-oriented selection has already applied
- Providing repeatable monitoring evidence when interface governance expects periodic confirmation of destination metadata freshness
- Complementing standard destination administration by packaging selection, time handling, and duration filtering in one pass

Typical use includes post-migration validation, periodic integration hygiene, and troubleshooting when remote systems were re-pointed or renamed. Teams still use standard RFC destination maintenance when a single destination needs immediate correction.

The routine selects from the destination and attribute join, computes duration using the configured unit relative to the evaluation date and time, and removes rows that do not satisfy the duration interval filter.
