"""
Fetch /SKN/S_SW_10_03_PO_APPR_BY_CR structure from SAP via pyrfc (DD03L + DD04T)
and save to input/Structure_PO_APPR_BY_CR_SW_10_03.xlsx.

Uses sw_th_fetch2's SAPRFCClient and query_ddic_structure (DDIC via RFC_READ_TABLE).
Run from repo root: python scripts/fetch_structure_po_appr_by_cr.py
Requires: pyrfc, pandas, openpyxl; SAP config at ../sw_th_fetch2/config/sap_config.json
"""
import sys
from pathlib import Path

# ei_auto_doc root
REPO_ROOT = Path(__file__).resolve().parent.parent
# sw_th_fetch2 (sibling of ei_auto_doc)
SW_FETCH_ROOT = REPO_ROOT.parent / "sw_th_fetch2"
if not SW_FETCH_ROOT.is_dir():
    print(f"[ERROR] sw_th_fetch2 not found at {SW_FETCH_ROOT}")
    sys.exit(1)
sys.path.insert(0, str(SW_FETCH_ROOT))

from src.sap_rfc_client import SAPRFCClient
from src.fetch_ei_info_batch import query_ddic_structure, save_structures_to_excel

STRUCTURE_NAME = "/SKN/S_SW_10_03_PO_APPR_BY_CR"
OUTPUT_PATH = REPO_ROOT / "input" / "Structure_PO_APPR_BY_CR_SW_10_03.xlsx"
CONFIG_PATH = SW_FETCH_ROOT / "config" / "sap_config.json"


def main():
    REPO_ROOT.mkdir(parents=True, exist_ok=True)
    (REPO_ROOT / "input").mkdir(parents=True, exist_ok=True)

    config_path = str(CONFIG_PATH) if CONFIG_PATH.exists() else None
    if not config_path:
        print(f"[ERROR] SAP config not found at {CONFIG_PATH}")
        sys.exit(1)

    rfc_client = SAPRFCClient(config_path=config_path)
    try:
        if not rfc_client.connect():
            print("[ERROR] Failed to connect to SAP")
            sys.exit(1)
        print(f"Querying DDIC for structure: {STRUCTURE_NAME}")
        fields = query_ddic_structure(STRUCTURE_NAME, rfc_client, abap_code="", function_name="")
        if not fields:
            print(f"[ERROR] No fields returned for {STRUCTURE_NAME}")
            sys.exit(1)
        save_structures_to_excel(fields, OUTPUT_PATH)
        print(f"Saved: {OUTPUT_PATH}")
    finally:
        rfc_client.disconnect()


if __name__ == "__main__":
    main()
