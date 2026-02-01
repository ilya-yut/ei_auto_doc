@echo off
cd /d "%~dp0..\.."
python scripts\pipeline\pipeline.py prepare
if errorlevel 1 exit /b 1
echo.
echo --- Next: open Cursor, then open this file and send the instruction inside it: ---
echo     scripts\pipeline\CURSOR_INSTRUCTION.txt
echo --- When the 7 response files exist in scripts\pipeline\run\, press any key to continue. ---
pause
echo --- Optional: run "python scripts\pipeline\pipeline.py verify" to check 04 rules before assemble. ---
python scripts\pipeline\pipeline.py assemble
if errorlevel 1 exit /b 1
echo.
echo Done. Check folder: output\
pause
