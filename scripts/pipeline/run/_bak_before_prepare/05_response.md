### Parameter Relationships

How parameter combinations work together

**Aggregation path:** **AGG_LVL** selects how differences are rolled up before detail lines are returned: empty (line level), **WERKS** (plant and posting date), or **IBLNR** (inventory document and fiscal year). **DIFF_AMOUNT** and **RESULT_COMP** define the symmetric difference band applied in having/selection logic.

**Date window:** When the monitor date range is empty, **BACKDAYS** is the fallback that builds a lower bound applied to the field named in **DATE_REF_FLD**; explicit date selections override that fallback.

**Duration filter:** After date selection, **DURATION** with **DURATION_UNIT** is an additional age filter on the reference date field named in **DATE_REF_FLD**.

**Difference scope:** **PRESENT_ZERO** controls whether zero **DMBTR** differences are excluded; when empty, non-zero differences are required.

**Comparison fields:** **REF_TABNAME1**, **REF_FIELD1**, **REF_TABNAME2**, **REF_FIELD2**, **COMP_OPERATOR**, and **WAERS_FR** configure optional cross-field amount comparison with currency handling via **WAERS**, **BWKEY**, **BUKRS**, and **KTOPL** when used.

**Organizational filters:** **VGART**, **WERKS**, **LGORT**, **SOBKZ**, status fields, and user parameters narrow which inventory documents and items enter the selection.

**Execution and text:** **SW_DEST** delegates to the cloud function when set; **LANGU** drives material descriptions; **MANAGE_IN_UTC** applies framework UTC handling when set.
