# Default Values section vs. ABAP code — Part 1 (converted docs)

**Scope:** All `*.md` files under `ei docs for analysis/Part 1 conv/` (33 EI documents).  
**Method:** The section **Default Values** (heading `### Default Values`, `## Default Values`, or plain `Default Values`) was parsed up to the next markdown heading or plain-title boundary (`Practical Example of Parameter Configuration`, `EI Function Structure`, etc.). ABAP was taken from the fenced block tagged `abap` that contains `FUNCTION …` (some Word exports had a spurious earlier fence without code).

**What “explicit default” means here:** Assignments such as `LV_DURATION_UNIT = 'M'`, `IF LV_BACKDAYS IS INITIAL. … LV_BACKDAYS = 1.`, `BACKDAYS = '1' . "--- Default`, `LV_MAXSEL = 500.`, and similar initialization *before* main selection/read logic. **Not** treated as configurable parameter defaults: `IS_ALERT = 'X'` (alert export), purely runtime counters (e.g. `LV_CNT = SY-TFILL`), or assignments inside downstream logic. Wrapper functions that only `CALL FUNCTION` another module may not contain the defaults locally — the doc text may still describe behavior implemented in the **callee** (noted per file).

---

## Summary table

| File name | Needs correction | Current Default Values section (abbrev.) | Corrected Default Values section (proposal) | Proof from code (where defaults are set) |
|-----------|------------------|------------------------------------------|---------------------------------------------|------------------------------------------|
| SW_01 _02_IDOCS - IDOCs State.md | No | Lists BACKDAYS=1, DURATION empty, DURATION_UNIT→M, … | *(unchanged)* | `LV_DURATION_UNIT = 'M'.`; `BACKDAYS = 1 .` default block in same ABAP fence |
| SW_01 _02_IDOCS_CNT - IDOCs Count.md | No | BACKDAYS, DURATION, DURATION_UNIT bullets | *(unchanged)* | Wrapper calls `/SKN/F_SW_01_02_IDOCS`; defaults mirror detail EI — see IDOCs State module for same patterns |
| SW_01_01_DUMPS - System Dumps (Details Monitoring).md | No | DURATION_UNIT→H, BACKDAYS→1, … | *(unchanged)* | `LV_DURATION_UNIT = 'H'.`; `BACKDAYS = 1 . "--- Default` in same function |
| SW_01_01_DUMPS_COUNT - System Dumps Count.md | No | Note + BACKDAYS/DURATION bullets | *(unchanged)* | Count wrapper; detailed defaults in called dumps monitor (same family as DUMPS detail EI) |
| SW_01_01_JOBS_CANC_N - Canceled Background Jobs Monitoring.md | No | BACKDAYS→1, DURATION/DURATION_UNIT | *(unchanged)* | `LV_DURATION_UNIT = 'M'.`; `BACKDAYS = '1' . "--- Default` |
| SW_01_01_JOBS_CNT - Background Jobs Count.md | No | Note + parameters | *(unchanged)* | Wrapper to job monitor FM; defaults in callee (same pattern as other *_CNT EIs) |
| SW_01_01_JOBS_STATE - Background Jobs Control.md | No | BACKDAYS from default, DURATION/DURATION_UNIT | *(unchanged)* | `LV_DURATION_UNIT = 'M'.`; `BACKDAYS = '1' . "--- Default` |
| SW_01_01_NUM_RNG - Number Range Control Monitoring.md | No | DURATION_UNIT, MANAGE_IN_UTC, … | *(unchanged)* | First LV init: `LV_LANGU = SY-LANGU.` (session); other defaults per field blocks in same ABAP fence |
| SW_01_01_SM12 - Application Locks Monitoring.md | No | DURATION / DURATION_UNIT | *(unchanged)* | `LV_DURATION_UNIT = 'M'.` |
| SW_01_01_SM13 - Update Requests Monitoring.md | No | BACKDAYS, DURATION, DURATION_UNIT | *(unchanged)* | `LV_DURATION_UNIT = 'M'.`; `IF LV_BACKDAYS IS INITIAL.` / `LV_BACKDAYS = 1.` in same fence |
| SW_01_01_SM13_CNT - Update Requests Count Monitoring.md | **Yes** | Says BACKDAYS “today + **tomorrow**” (wording error) | Replace bullet with: `· BACKDAYS - initial - treated as 1 by code (today and yesterday; one-day lookback per glossary).` | **Wrapper only** `CALL FUNCTION '/SKN/F_SW_01_01_SM13'` — BACKDAYS/DURATION defaults **not** in this listing; align wording with glossary in same doc (line 118). Typo is independent of callee. |
| SW_01_01_SM50 - Work Process Monitoring.md | No | DURATION empty, DURATION_UNIT→M | *(unchanged)* | `LV_DURATION_UNIT = 'M'.` |
| SW_01_01_SYS_BDC - Batch Input Monitoring.md | No | BACKDAYS, DURATION, DURATION_UNIT | *(unchanged)* | `LV_DURATION_UNIT = 'M'.` |
| SW_01_01_SYS_IS_OPEN - System Client Status Monitoring.md | No | DURATION unconstrained, DURATION_UNIT→D | *(unchanged)* | `LV_DURATION_UNIT = 'D'.` |
| SW_01_02_LBWQ_CNT - Logistics Queue Overview.md | No | “No default values…” | *(unchanged)* | No active `LV_DURATION_UNIT` / BACKDAYS init in non-comment code (commented legacy only) |
| SW_01_02_MSG_STATUS - Message Status Monitoring.md | No | BACKDAYS→1, DURATION, DURATION_UNIT→M | *(unchanged)* | `LV_DURATION_UNIT = 'M'.`; `BACKDAYS = 1 .` / date window logic in same function |
| SW_01_02_QRFC_IN_CNT - qRFC Inbound Queue Monitoring.md | No | “No default values…” | *(unchanged)* | No parameter LV defaults in active code (BACKDAYS block commented); `IS_ALERT` only at end |
| SW_01_02_QRFC_OUT_CN - qRFC Outbound Queue Monitoring.md | No | “No default values…” | *(unchanged)* | Same pattern as inbound count wrapper |
| SW_01_02_RFC_PING - RFC Destination Connectivity Monitoring_.md | No | “No default values…” | *(unchanged)* | No `LV_DURATION_UNIT` line in excerpted function (ping logic only) |
| SW_01_02_RFC_STATE - RFC Destination Configuration Staleness Monitoring.md | No | DURATION_UNIT→D, … | *(unchanged)* | `LV_DURATION_UNIT = 'D'.` |
| SW_01_02_SOST - SAPconnect Send Requests Monitoring.md | **Yes** | *(section missing in Word export)* | Add **### Default Values** with bullets: (1) `LV_DURATION_UNIT = 'M'` before `SELECT_SINGLE`; (2) `LV_BACKDAYS` set to `1` when initial; (3) `LV_MAXSEL = 500` when max records not set; (4) when `LS_STATUS` initial, multiple status flags set to `X` after `SET_STATUS`; (5) optional note on `LV_LANGU = SY-LANGU`. | `LV_DURATION_UNIT = 'M'.` **line 756**; `IF LV_BACKDAYS IS INITIAL.` / `LV_BACKDAYS = 1.` **783–784**; `LS_STATUS-WAIT = 'X'.` … **814–818**; `LV_MAXSEL = 500.` **821** (`ei docs for analysis/Part 1 conv/SW_01_02_SOST - SAPconnect Send Requests Monitoring.md`) |
| SW_01_02_SOST_CNT - SAPconnect Send Requests Count Monitoring.md | No | BACKDAYS/DURATION/DURATION_UNIT + note re: detail routine | *(unchanged)* | Doc note: defaults live in **`/SKN/F_SW_01_02_SOST`** (see **SOST** file lines above); this wrapper only forwards `T_SELECT` / `T_DATA` |
| SW_01_02_TRFC - tRFC Transactional RFC Monitoring.md | No | DURATION unset, DURATION_UNIT→M | *(unchanged)* | `LV_DURATION_UNIT = 'M'.` in ABAP fence |
| SW_01_02_XI_MESS - XI Message Monitoring.md | No | DURATION / DURATION_UNIT | *(unchanged)* | `LV_DURATION_UNIT = 'M'.` |
| SW_01_04_DISK_FREE_E - Enhanced Free Disk Space Monitoring.md | No | Path / threshold defaults narrative | *(unchanged)* | `LV_DEST = LV_SW_DEST.` (cloud path handoff); see full ABAP fence for disk-specific init |
| SW_01_20_SM66 - Global Work Process Overview.md | No | Same family as SM50 | *(unchanged)* | `LV_DURATION_UNIT = 'M'.` |
| SW_01_20_SRV_CPU - Server CPU Performance Monitoring.md | **Yes** | “No default values are defined for this EI.” | Replace with bullets aligned with other monitors, e.g. `· DURATION_UNIT - initial - treated as M by code (LV_DURATION_UNIT preset before SELECT_SINGLE).` Add note if duration **delete** logic is commented out but unit still preset. | `LV_DURATION_UNIT = 'M'.` **line 427** in same `.md` |
| SW_01_20_SRV_MEM - Server Memory Performance Monitoring.md | **Yes** | “No default values are defined for this EI.” | Same pattern as SRV_CPU: document `LV_DURATION_UNIT = 'M'` preset. | `LV_DURATION_UNIT = 'M'.` **line 430** in same `.md` |
| SW_01_20_USR_PWD - User Password State Monitoring.md | No | DURATION_UNIT→D, … | *(unchanged)* | `LV_DURATION_UNIT = 'D'.` |
| SW_01_20_WP_TOT - System Work Processes Count Monitoring.md | No | Aggregation defaults | *(unchanged)* | `LV_AGGR_LEVEL = 'T'.` (first distinctive LV init in fence) |
| SW_01_AMS_BUFF_STAT - AMS Buffer Statistics Monitoring_.md | No | AMS-specific bullets | *(unchanged)* | No `LV_DURATION_UNIT` in short fence — defaults are AMS-specific constants; see ABAP fence in file |
| SW_01_AMS_WORKLOAD - AMS Workload Indicator.md | No | DURATION_UNIT→D, … | *(unchanged)* | `LV_DURATION_UNIT = 'D'.` |
| SW_AUD_SM20_TR_START - Transaction Start Monitoring_.md | No | DURATION / DURATION_UNIT | *(unchanged)* | `LV_DURATION_UNIT = 'D'.` |

---

## Files requiring edits (short list)

1. **SW_01_02_SOST - SAPconnect Send Requests Monitoring.md** — Add a full **Default Values** subsection; proof lines **756–758, 783–785, 813–819, 821–823** in `Part 1 conv` export.
2. **SW_01_01_SM13_CNT - Update Requests Count Monitoring.md** — Fix **“tomorrow” → “yesterday”**; optionally add footnote that numeric defaults are enforced inside **`/SKN/F_SW_01_01_SM13`**, not in the count wrapper.
3. **SW_01_20_SRV_CPU** and **SW_01_20_SRV_MEM** — Replace “no defaults” with **DURATION_UNIT → M** via `LV_DURATION_UNIT = 'M'.` (lines **427** / **430**), consistent with other EIs.

---

## Tooling

- `tools/validate_default_values_vs_code.py` — parses sections, extracts ABAP fence containing `FUNCTION`, collects assignment evidence; writes `default_values_validation.json` in the same folder.

Run:

```text
python tools/validate_default_values_vs_code.py
```
