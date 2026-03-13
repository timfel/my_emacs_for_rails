@echo off
setlocal enabledelayedexpansion

rem Configure this repository to use the versioned hooks in .githooks\
rem and set core.hookEmacs so hooks can find Emacs on Windows.
rem
rem Strategy:
rem   1) If emacs.exe is on PATH, use "emacs".
rem   2) Otherwise, run PowerShell WITH profile to resolve the actual emacs.exe path
rem      (so a profile-defined `emacs` function can be used), and store that.

where git >nul 2>nul
if errorlevel 1 (
  echo git not found in PATH
  exit /b 1
)

git rev-parse --is-inside-work-tree >nul 2>nul
if errorlevel 1 (
  echo Not inside a git repository.
  exit /b 1
)

rem Set hooks path
git config core.hooksPath .githooks
if errorlevel 1 (
  echo Failed to set core.hooksPath
  exit /b 1
)

set "EMACS_RES="

rem 1) Check PATH
where emacs.exe >nul 2>nul
if not errorlevel 1 (
  set "EMACS_RES=emacs"
) else (
  rem 2) Use PowerShell with profile to resolve emacs executable path
  rem We ask PowerShell: resolve whatever `emacs` points to (alias/function/app),
  rem then run it with -Q and an elisp snippet that prints invocation-name.
  for /f "usebackq delims=" %%I in (`
    powershell -NoLogo -ExecutionPolicy Bypass -Command "emacs -Q --batch --eval '(princ (expand-file-name invocation-name invocation-directory))'"
  `) do (
    set "EMACS_RES=%%I"
  )
)

if "%EMACS_RES%"=="" (
  echo Could not resolve Emacs. Ensure emacs.exe is on PATH or that your PowerShell profile defines an emacs function that runs Emacs.
  exit /b 1
)

git config core.hookEmacs "%EMACS_RES%"
if errorlevel 1 (
  echo Failed to set core.hookEmacs
  exit /b 1
)

echo Configured:
echo   core.hooksPath = .githooks
echo   core.hookEmacs = %EMACS_RES%
endlocal
