"""Regenerate only 04_prompt.txt from template (no response files deleted). Run from repo root."""
from pathlib import Path
import re
import sys

PIPELINE_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = PIPELINE_DIR.parent.parent
PROMPTS_DIR = PROJECT_ROOT / "prompts"
RUN_DIR = PIPELINE_DIR / "run"
INPUT_DIR = PROJECT_ROOT / "input"

# Reuse pipeline's discover and placeholder logic
sys.path.insert(0, str(PIPELINE_DIR))
from pipeline import _discover_inputs  # noqa: E402

# We need replace_placeholders which is defined inside prepare(); get paths and replicate logic
paths = _discover_inputs(assume_yes=True)
if not paths:
    print("Input files not found.", file=sys.stderr)
    sys.exit(1)


def _repl(s):
    return str(s).replace("\\", "\\\\")


def replace(text: str) -> str:
    text = re.sub(r"\[Provide the structure file path[^\]]*\]", _repl(paths["structure"]), text)
    text = re.sub(r"\[Provide the output structure / fields file path[^\]]*\]", _repl(paths["structure"]), text)
    text = re.sub(r"\[Provide the Parameters sheet path[^\]]*\]", _repl(paths["params"]), text)
    text = re.sub(r"\[Provide the file path or paste the parameters table content here\]", _repl(paths["params"]), text)
    text = re.sub(r"\[Provide the code file path[^\]]*\]", _repl(paths["code"]), text)
    text = re.sub(r"\[Provide the ABAP source\]", _repl(paths["code"]), text)
    if paths.get("params_docx"):
        text = re.sub(
            r"Selected parameters file[^\n]*\n[^\n]*",
            "Selected parameters file: " + _repl(paths["params_docx"]) + "\n",
            text,
            count=1,
        )
    else:
        text = re.sub(
            r"Selected parameters file[^\n]*\n[^\n]*",
            "Selected parameters file: (optional – omit if not in input)\n",
            text,
            count=1,
        )
    return text


template_path = PROMPTS_DIR / "PROMPT_Parameter_Configuration_Guidelines_section.md"
text = template_path.read_text(encoding="utf-8")
text = replace(text)
(RUN_DIR / "04_prompt.txt").write_text(text, encoding="utf-8")
print("Wrote", RUN_DIR / "04_prompt.txt")
