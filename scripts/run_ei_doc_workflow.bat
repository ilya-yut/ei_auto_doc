@echo off
setlocal
:: EI Doc workflow (Cursor in-IDE narrative, no API key).
:: Step 1: script writes skeleton + narrative_context. Step 2: you fill TODOs in Cursor using that context.

set "SCRIPT_DIR=%~dp0"
set "PROJECT_ROOT=%SCRIPT_DIR:~0,-1%\.."
set "PY_EXE=python"
set "GENERATOR=%SCRIPT_DIR%generate_ei_doc.py"

echo.
echo === EI Doc Workflow (Cursor in-IDE, no API key) ===
echo Project root: %PROJECT_ROOT%
echo.

:: ----- Pre-checks -----
echo [1/4] Pre-checks...
where %PY_EXE% >nul 2>&1
if errorlevel 1 (
    echo FAIL: Python not found. Install Python and ensure it is on PATH.
    exit /b 1
)
if not exist "%PROJECT_ROOT%\input" (
    echo FAIL: Missing folder: %PROJECT_ROOT%\input
    exit /b 1
)
if not exist "%PROJECT_ROOT%\reference" (
    echo FAIL: Missing folder: %PROJECT_ROOT%\reference
    exit /b 1
)
if not exist "%PROJECT_ROOT%\reference\sample_format.md" (
    echo FAIL: Missing reference\sample_format.md
    exit /b 1
)
if not exist "%PROJECT_ROOT%\reference\_Parameters.docx" (
    echo WARN: reference\_Parameters.docx not found. Parameter descriptions may be generic.
)
echo   OK: Python, input/, reference/ present.

:: ----- Run generator (--cursor-workflow: skeleton + narrative_context) -----
echo.
echo [2/4] Running: python scripts\generate_ei_doc.py --cursor-workflow
cd /d "%PROJECT_ROOT%"
"%PY_EXE%" "%GENERATOR%" --cursor-workflow
if errorlevel 1 (
    echo FAIL: Generator exited with error.
    exit /b 1
)
echo   OK: Generator finished. Skeleton and narrative_context written.

:: ----- Verify: skeleton + context -----
echo.
echo [3/4] Verifying output...
if not exist "%PROJECT_ROOT%\output" (
    echo FAIL: output folder not found.
    exit /b 1
)
set "MAIN_DOC="
set "CTX_DOC="
for %%F in ("%PROJECT_ROOT%\output\Explanation_*.md") do (
    echo %%~nxF | findstr /i "_narrative_context" >nul 2>&1
    if errorlevel 1 set "MAIN_DOC=%%~F"
    if not errorlevel 1 set "CTX_DOC=%%~F"
)
if not defined MAIN_DOC (
    echo FAIL: No main output doc found.
    exit /b 1
)
if not defined CTX_DOC (
    echo FAIL: No narrative_context file found.
    exit /b 1
)
findstr /C:"[TODO: General Overview]" "%MAIN_DOC%" >nul 2>&1
if errorlevel 1 goto :verify_warn
echo   OK: Skeleton with [TODO:] and narrative_context present.
goto :verify_done
:verify_warn
echo WARN: Main doc has no [TODO: General Overview]. Re-run with --cursor-workflow if you want Cursor fill.
:verify_done
echo   OK: output\Explanation_*.md and *_narrative_context.md exist.

:: ----- Step 2: fill in Cursor -----
echo.
echo [4/4] Step 2 (do this in Cursor)
echo ----------------------------------------
echo The generator printed a "Next: Ask Cursor to ..." line above.
echo In Cursor, paste that line as your prompt, or say:
echo.
echo   Fill all [TODO:] in output\Explanation_(your doc).md using output\Explanation_(your doc)_narrative_context.md and reference/
echo.
echo Cursor will draft Overview, Problem, Resolution from the context and rules.
echo ----------------------------------------
echo.
echo === Workflow step 1 complete. Do step 2 in Cursor to get tailored narrative. ===
echo.
endlocal
exit /b 0
