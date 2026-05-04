### Default Values and Practical Examples

### Default Values

- `DURATION_UNIT = D` is a practical baseline for daily audit monitoring.
- If no explicit date is provided, a recent-day window is commonly used to keep volume manageable.
- Package/window slicing should match expected event volume to prevent oversized pulls.

### Practical Configuration Examples

Use Case 1: Daily monitoring for critical transactions

```ini
DURATION=1
DURATION_UNIT=D
MSCDATE=20260325-20260326
TCODE=SU01
USER=*
```

Use Case 2: Off-hours transaction start review

```ini
DURATION=12
DURATION_UNIT=H
MSCDATE=20260325-20260325
MSCTIME=200000-235959
TRN_EX=SE38
```

Use Case 3: Fast triage for a specific user set

```ini
DURATION=90
DURATION_UNIT=M
USERID=FIN_AUDITOR_01
TCODE=SM20
```
