# DATE_REF_FLD — documented values vs. code (Part 1)

**Scope:** All `*.md` files under `ei docs for analysis/Part 1 conv/` (33 EI documents).

**Method**

- **Doc side:** `DATE_REF_FLD` parameter text and the **DATE_REF_FLD Options:** bullet list (when present).
- **Code side:** Inlined ABAP in the same file: `SELECT_SINGLE: DATE_REF_FLD`, `IF LV_DATE_REF_FLD IS INITIAL. LV_DATE_REF_FLD = '…'.`, and the first `CASE LV_DATE_REF_FLD. … ENDCASE` that maps the lookback window (`R_DATUM[]` → `R_STRTDATE[]` / `R_PWDCHGDATE[]` / etc.). For **count wrappers**, the delegated function name is cited (`CALL FUNCTION '…'`).
- **Corrected / proposal column:** Comma-separated **`WHEN`** literals (plus **`OTHERS`** if present) and **`initial` → `…`** when that default exists in pasted code; otherwise one short line stating that **no `CASE LV_DATE_REF_FLD` / no literals** exist in the pasted ABAP, or that **`DATE_REF_FLD` does not appear** in that export. *(Wrapper-only EIs: literals are taken from the delegated function named in **Proof**.)*

**Tooling:** `tools/validate_date_ref_fld_vs_code.py` writes `date_ref_fld_validation.json` (doc option lines + `WHEN` literals from the first `CASE LV_DATE_REF_FLD` block).

---

## Summary table

| File name | Needs correction | Current DATE_REF_FLD / options (abbrev.) | Corrected / proposal | Proof from code |
|-----------|------------------|------------------------------------------|------------------------|-----------------|
| SW_01 _02_IDOCS - IDOCs State.md | **Yes** | Options: CREDAT, UPDDAT, plus vague “code-defined mapping”. | **No `CASE LV_DATE_REF_FLD` / no `WHEN` values in pasted code** | No `CASE LV_DATE_REF_FLD`, no `FIELDNM = 'DATE_REF_FLD'` in ABAP fence. Lookback: `R_CREDAT[] = R_DATUM[]` when both `R_CREDAT`/`R_UPDDAT` empty (**887–890**). **Duration always uses `T_DATA-UPDDAT`** in `F_SW_GET_TIME_DIFF` **`D_FROM`** (**956–958**), not `DATE_REF_FLD`. |
| SW_01 _02_IDOCS_CNT - IDOCs Count.md | **Yes** | Options: DATUM, UPDDAT. | **No `CASE LV_DATE_REF_FLD` in wrapper; none in pasted `/SKN/F_SW_01_02_IDOCS`** | Wrapper calls **`/SKN/F_SW_01_02_IDOCS`** (**443**); no `CASE LV_DATE_REF_FLD` in wrapper. |
| SW_01_01_DUMPS - System Dumps (Details Monitoring).md | — | *(no `DATE_REF_FLD` in this export)* | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_01_DUMPS_COUNT - System Dumps Count.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_01_JOBS_CANC_N - Canceled Background Jobs Monitoring.md | **Yes** | Parameter reference table lists **dump-style** fields only; **no** `DATE_REF_FLD` subsection. | **`STRTDATE`, `ENDDATE`, `SDLDATE`, `SDLSTRTDT`, `LASTCHDATE`, `RELDATE`, `OTHERS`; `initial` → `STRTDATE`** | ABAP is **`/SKN/F_SW_01_01_JOBS_N`** with `DATE_REF_FLD` in `SELECT_SINGLE`, default **`LV_DATE_REF_FLD = 'STRTDATE'`** when initial (**313–315**), first **`CASE LV_DATE_REF_FLD`** **367–382**, second **`CASE`** for duration ref time **471–486**. |
| SW_01_01_JOBS_CNT - Background Jobs Count.md | No | Full job date columns + OTHERS → STRTDATE. | **`STRTDATE`, `ENDDATE`, `SDLDATE`, `SDLSTRTDT`, `LASTCHDATE`, `RELDATE`, `OTHERS`; `initial` → `STRTDATE`** | Delegates to **`/SKN/F_SW_01_01_JOBS_N`**; options match **JOBS_STATE** / **JOBS_CANC_N** `CASE` set. |
| SW_01_01_JOBS_STATE - Background Jobs Control.md | No | STRTDATE, ENDDATE, SDLDATE, SDLSTRTDT, LASTCHDATE, RELDATE, OTHERS. | **`STRTDATE`, `ENDDATE`, `SDLDATE`, `SDLSTRTDT`, `LASTCHDATE`, `RELDATE`, `OTHERS`; `initial` → `STRTDATE`** | `IF LV_DATE_REF_FLD IS INITIAL … 'STRTDATE'.` (**554–556**); first `CASE LV_DATE_REF_FLD` **608–623**; duration ref-time `CASE` **712–727** — same literals. |
| SW_01_01_NUM_RNG - Number Range Control Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_01_SM12 - Application Locks Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_01_SM13 - Update Requests Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_01_SM13_CNT - Update Requests Count Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_01_SM50 - Work Process Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_01_SYS_BDC - Batch Input Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_01_SYS_IS_OPEN - System Client Status Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_02_LBWQ_CNT - Logistics Queue Overview.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_02_MSG_STATUS - Message Status Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_02_QRFC_IN_CNT - qRFC Inbound Queue Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_02_QRFC_OUT_CN - qRFC Outbound Queue Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_02_RFC_PING - RFC Destination Connectivity Monitoring_.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_02_RFC_STATE - RFC Destination Configuration Staleness Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_02_SOST - SAPconnect Send Requests Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_02_SOST_CNT - SAPconnect Send Requests Count Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_02_TRFC - tRFC Transactional RFC Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_02_XI_MESS - XI Message Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_04_DISK_FREE_E - Enhanced Free Disk Space Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_20_SM66 - Global Work Process Overview.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_20_SRV_CPU - Server CPU Performance Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_20_SRV_MEM - Server Memory Performance Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_20_USR_PWD - User Password State Monitoring.md | No | PWDCHGDATE, PWDLGNDATE, PWDSETDATE, PWDLOCKDATE. | **`PWDCHGDATE`, `PWDLGNDATE`, `PWDSETDATE`, `PWDLOCKDATE`, `OTHERS`** | First `CASE LV_DATE_REF_FLD` **621–633** (incl. `OTHERS` → `R_PWDCHGDATE`); duration `CASE` **733–742** (no `OTHERS`; **`REF_DATE`** primed with **`T_DATA-PWDCHGDATE`** at **731**). |
| SW_01_20_WP_TOT - System Work Processes Count Monitoring.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_AMS_BUFF_STAT - AMS Buffer Statistics Monitoring_.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_01_AMS_WORKLOAD - AMS Workload Indicator.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |
| SW_AUD_SM20_TR_START - Transaction Start Monitoring_.md | — | — | **No `DATE_REF_FLD` in doc or pasted code** | — |

---

## Files requiring edits (short list)

1. **SW_01 _02_IDOCS - IDOCs State.md** — Documented `DATE_REF_FLD` options are **not implemented** in the shown function; aging uses **UPDDAT/UPDTIM** only.
2. **SW_01 _02_IDOCS_CNT - IDOCs Count.md** — Same issue via delegation to **`/SKN/F_SW_01_02_IDOCS`**.
3. **SW_01_01_JOBS_CANC_N - Canceled Background Jobs Monitoring.md** — **Parameter table / guidelines do not match** the pasted job ABAP (missing `DATE_REF_FLD` and job fields; looks like a template mix-up with a dump-count layout).

---

## Tooling

```text
python tools/validate_date_ref_fld_vs_code.py
```

Writes `date_ref_fld_validation.json` in `Part 1 conv/`.

**Note:** Some `.md` files use a **replacement character (U+FFFD)** instead of a proper bullet before option lines; the script tolerates that pattern so option lists still parse.
