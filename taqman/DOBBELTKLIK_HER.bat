@echo off
REM Kører et R script. Put denne fil i den samme folder som dit script eller ændre SCRIPT= til filens path.

set SCRIPT=script_taqman_QS.R

REM Sørg for at det her er den rigtige placering og version (R-4.6.0) af R
set RSCRIPT="C:\Program Files\R\R-4.6.0\bin\Rscript.exe"

cd /d "%~dp0"
%RSCRIPT% "%SCRIPT%"

echo.
echo Slut (exit code %ERRORLEVEL%).
pause