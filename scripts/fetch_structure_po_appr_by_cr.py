"""
Fetch a DDIC structure from SAP via pyrfc (DD03L + DD04T) and save to input/Structure_<name>.xlsx.

Uses sw_th_fetch2's SAPRFCClient and query_ddic_structure (DDIC via RFC_READ_TABLE).
Run from repo root:
  python scripts/fetch_structure_po_appr_by_cr.py [STRUCTURE_NAME]
  e.g. python scripts/fetch_structure_po_appr_by_cr.py /SKN/S_SW_10_03_PR_PO_VAL_CHK

Default (no arg): /SKN/S_SW_10_03_PO_APPR_BY_CR
Requires: pyrfc, pandas, openpyxl; SAP config at ../sw_th_fetch2/config/sap_config.json
"""
import sys
import re
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

DEFAULT_STRUCTURE = "/SKN/S_SW_10_03_PO_APPR_BY_CR"
CONFIG_PATH = SW_FETCH_ROOT / "config" / "sap_config.json"


def structure_to_filename(structure_name: str) -> str:
    """e.g. /SKN/S_SW_10_03_PR_PO_VAL_CHK -> Structure_PR_PO_VAL_CHK_SW_10_03.xlsx"""
    name = structure_name.strip().strip("/")
    # Structure_<suffix>.xlsx; keep last part meaningful
    suffix = re.sub(r"[/\s]+", "_", name)
    return f"Structure_{suffix}.xlsx"


def main():
    structure_name = (sys.argv[1] if len(sys.argv) > 1 else DEFAULT_STRUCTURE).strip()
    if not structure_name.startswith("/"):
        structure_name = "/SKN/" + structure_name.lstrip("S_")
    output_path = REPO_ROOT / "input" / structure_to_filename(structure_name)

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
        print(f"Querying DDIC for structure: {structure_name}")
        fields = query_ddic_structure(structure_name, rfc_client, abap_code="", function_name="")
        if not fields:
            print(f"[ERROR] No fields returned for {structure_name}")
            sys.exit(1)
        save_structures_to_excel(fields, output_path)
        print(f"Saved: {output_path}")
    finally:
        rfc_client.disconnect()


if __name__ == "__main__":
    main()
