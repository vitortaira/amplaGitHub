@echo off
echo Iniciando aplicacao Shiny...
cd /d "%~dp0"
R -e "source('run_locally.R')"
pause
