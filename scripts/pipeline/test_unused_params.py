"""Quick checks for unused_params analyzer."""
from __future__ import annotations

import sys
from pathlib import Path

PIPELINE_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(PIPELINE_DIR))
from unused_params import analyze_unused_params  # noqa: E402

ROOT = PIPELINE_DIR.parents[1]
INPUT = ROOT / "input"
OLD = INPUT / "old"
DIRS = [INPUT, OLD]


def _paths(stem: str) -> tuple[Path, Path]:
    for base in (INPUT, OLD):
        code = base / f"Code_{stem}.txt"
        params = base / f"Available fields_{stem}.xlsx"
        if code.exists() and params.exists():
            return code, params
    raise FileNotFoundError(stem)


def test_open_po_det_none_unused() -> None:
    code, params = _paths("SKN_S_SW_10_03_OPEN_PO_DET")
    assert analyze_unused_params(code, params, search_dirs=DIRS) == set()


def test_po_tot_val_frgco_unused() -> None:
    code, params = _paths("ZSWS_CBC_10_03_PO_PER_TOT_VAL")
    unused = analyze_unused_params(code, params, search_dirs=DIRS)
    assert "FRGCO" in unused


def test_po_agree_datum_only() -> None:
    code, params = _paths("SKN_S_SW_10_02_PO_AGREE_VALID")
    unused = analyze_unused_params(code, params, search_dirs=DIRS)
    assert unused == {"DATUM"}


if __name__ == "__main__":
    test_open_po_det_none_unused()
    test_po_tot_val_frgco_unused()
    test_po_agree_datum_only()
    print("ok")
