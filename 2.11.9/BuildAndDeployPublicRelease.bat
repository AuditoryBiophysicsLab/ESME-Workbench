@echo off
echo This script will build and release the current version of ESME Workbench 
echo to the world.
echo.
echo Please be sure you really want to do this!
echo.
set /p UserConfirmation="Are you sure? (type 'yes' to confirm) "
if "%UserConfirmation%"=="yes" goto ProceedWithBuild
goto DoNotBuild
:ProceedWithBuild
echo Starting build
call "BuildAndDeploy.bat" download
goto End
:DoNotBuild
echo Build canceled
:End