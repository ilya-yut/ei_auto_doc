# USER_FLD “possible values” — doc text vs. code / structure (Part 1)

**Scope:** All `*.md` files under `ei docs for analysis/Part 1 conv/` (33 EI documents).

**Method**

- **Doc “possible values”** is interpreted as: (a) the **USER_FLD** parameter subsection (standard DRL narrative + any **USER_FLD Options:** bullet list), and (b) where present, **Practical Example** lines of the form `USER_FLD = <FIELD>`.
- **Code / structure proof:** In these EIs, `USER_FLD` is almost never referenced by name inside the shown `FUNCTION` body; routing uses the **Dynamic Recipient List** pattern (value = **technical name of a column** on the monitor’s **`T_DATA` / output structure**). Proof is therefore taken from:
  - **`DATA_MULTY:` / `SELECT_MULTY:`** and **`DELETE T_DATA WHERE …`** clauses (which fields exist on the result row), and/or  
  - The **EI Function Structure** table in the same file (pipe tables `| /SKN/S_… | FIELDNAME | … |`).

**Important:** Only **SW_01_01_SM13** uses an explicit **USER_FLD Options:** list. Other Part 1 files use the shared DRL boilerplate without an enumerated allowed set in the prose.

---

## Summary table

| File name | Needs correction | Current USER_FLD / “possible values” text (abbrev.) | Corrected text (proposal) | Proof from code / structure |
|-----------|------------------|------------------------------------------------------|----------------------------|-------------------------------|
| SW_01 _02_IDOCS - IDOCs State.md | No | Generic DRL narrative; no fixed option list | *(unchanged)* | No `USER_FLD` literal in ABAP fence; usable values = components of `/SKN/S_…` output (see **EI Function Structure** + `DATA_MULTY` / `SELECT_MULTY` in same `.md`) |
| SW_01 _02_IDOCS_CNT - IDOCs Count.md | — | *(no USER_FLD subsection in this export)* | — | Parameter not documented in this Part 1 file |
| SW_01_01_DUMPS - System Dumps (Details Monitoring).md | No | Generic DRL narrative | *(unchanged)* | Same pattern as IDOCS; structure + multy blocks in ABAP fence |
| SW_01_01_DUMPS_COUNT - System Dumps Count.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_01_JOBS_CANC_N - Canceled Background Jobs Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_01_JOBS_CNT - Background Jobs Count.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_01_JOBS_STATE - Background Jobs Control.md | No | Generic DRL narrative | *(unchanged)* | ABAP uses job-related `DATA_MULTY` fields; `USER_FLD` not literal in fence — map to output structure fields |
| SW_01_01_NUM_RNG - Number Range Control Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_01_SM12 - Application Locks Monitoring.md | No | Generic DRL narrative | *(unchanged)* | `DATA_MULTY` / `SELECT_MULTY` list lock-related columns; no `USER_FLD` string in fence |
| SW_01_01_SM13 - Update Requests Monitoring.md | **Yes** | **USER_FLD Options** lists **BREPORT** (among VBUSR, VBMANDT, …) | Replace **BREPORT** with **`VBREPORT`** (same meaning as in table “Generating program”); keep other lines or extend with additional `VB*` columns from structure if you want an exhaustive hint list | Output structure includes **`VBREPORT`** (`EI Function Structure` row **VBREPORT**); ABAP **`DATA_MULTY` / `SELECT_MULTY` / `DELETE T_DATA`** use **`VBREPORT`** — **lines 445–446, 477–491, 558** in `SW_01_01_SM13 - Update Requests Monitoring.md`. **BREPORT** does not appear as a component |
| SW_01_01_SM13_CNT - Update Requests Count Monitoring.md | — | *(no USER_FLD subsection)* | — | Wrapper only (`CALL FUNCTION '/SKN/F_SW_01_01_SM13'`) |
| SW_01_01_SM50 - Work Process Monitoring.md | No | Generic DRL narrative | *(unchanged)* | SM66 family: work-process columns in `/SKN/S_SW_01_01_SM66` (see structure table + `DATA_MULTY` in ABAP) |
| SW_01_01_SYS_BDC - Batch Input Monitoring.md | No | Generic DRL narrative | *(unchanged)* | BDC output fields in structure + multy blocks |
| SW_01_01_SYS_IS_OPEN - System Client Status Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_02_LBWQ_CNT - Logistics Queue Overview.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_02_MSG_STATUS - Message Status Monitoring.md | No | Generic DRL narrative | *(unchanged)* | Message row fields per structure + `SELECT_MULTY` in ABAP |
| SW_01_02_QRFC_IN_CNT - qRFC Inbound Queue Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_02_QRFC_OUT_CN - qRFC Outbound Queue Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_02_RFC_PING - RFC Destination Connectivity Monitoring_.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_02_RFC_STATE - RFC Destination Configuration Staleness Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_02_SOST - SAPconnect Send Requests Monitoring.md | No | Generic DRL + examples `USER_FLD = SENDER` / `RC_NAME` | *(unchanged)* | **`SENDER`** and **`RC_NAME`** are real columns on `/SKN/S_SW_01_02_SOST` (structure rows **~655–662**); `SELECT_MULTY` includes **`SENDER`** (**line ~751**) |
| SW_01_02_SOST_CNT - SAPconnect Send Requests Count Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_02_TRFC - tRFC Transactional RFC Monitoring.md | **Yes** | Generic DRL + **Use Case** `USER_FLD = EMAIL_ADDR` | Change example to a field that exists on **`/SKN/S_SW_01_02_TRFC`** in this document (e.g. **`ARFCUSER`**) unless you extend the Word export so **EI Function Structure** actually lists `EMAIL_ADDR` | In this export, **EI Function Structure** lists **ARFCUSER**, **ARFCDEST**, … up through **STATE_ICON** (**~351–455**) — **`EMAIL_ADDR` does not appear** anywhere except that example (**~328**). ABAP fence references structure `/SKN/S_SW_01_02_TRFC` (**~464**) |
| SW_01_02_XI_MESS - XI Message Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_04_DISK_FREE_E - Enhanced Free Disk Space Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_20_SM66 - Global Work Process Overview.md | No | Generic DRL + example `USER_FLD = WP_BNAME` | *(unchanged)* | **`WP_BNAME`** in structure (**~466**) and parameter table (**~169**) |
| SW_01_20_SRV_CPU - Server CPU Performance Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_20_SRV_MEM - Server Memory Performance Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_20_USR_PWD - User Password State Monitoring.md | No | Generic DRL + examples **`BNAME`**, **`CLASS`**, **`USTYP`** | *(unchanged)* | All three are columns on **`/SKN/S_SW_01_20_USER_PWD_STATE`** (see structure rows **BNAME**, **CLASS**, **USTYP** in same file) |
| SW_01_20_WP_TOT - System Work Processes Count Monitoring.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_AMS_BUFF_STAT - AMS Buffer Statistics Monitoring_.md | — | *(no USER_FLD subsection)* | — | — |
| SW_01_AMS_WORKLOAD - AMS Workload Indicator.md | — | *(no USER_FLD subsection)* | — | — |
| SW_AUD_SM20_TR_START - Transaction Start Monitoring_.md | — | *(no USER_FLD subsection)* | — | — |

---

## Files requiring edits (short list)

1. **SW_01_01_SM13 - Update Requests Monitoring.md** — Under **USER_FLD Options**, rename **`BREPORT`** → **`VBREPORT`** to match component **`VBREPORT`** used in `DATA_MULTY`, `SELECT_MULTY`, and `DELETE T_DATA` logic.
2. **SW_01_02_TRFC - tRFC Transactional RFC Monitoring.md** — Fix **Practical Example** `USER_FLD = EMAIL_ADDR`: **`EMAIL_ADDR`** is not listed in the **EI Function Structure** section of this export; use a documented field (e.g. **`ARFCUSER`**) or extend the structure documentation if the runtime type really includes more columns than the table shows.

---

## Tooling

- `tools/validate_user_fld_vs_code.py` — detects `USER_FLD Options:` lines, whether ABAP fence contains the literal `USER_FLD`, and parses **pipe-format** EI structure tables into field names; writes `user_fld_validation.json` beside this report.

Run:

```text
python tools/validate_user_fld_vs_code.py
```

**Note:** Some Word exports use **non-pipe** “EI Function Structure” tables (e.g. TRFC), so `structure_field_count` may be **0** in JSON even though the prose table exists — use the `.md` source for that EI when validating examples.
