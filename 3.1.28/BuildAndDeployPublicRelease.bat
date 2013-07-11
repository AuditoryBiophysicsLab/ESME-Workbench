@echo off
echo This script will build and release the current version of ESME Workbench 
echo to the world.
echo.
echo Please be sure you really want to do this!
echo.
echo Have you updated the minor version number (if appropriate)?
echo.
set /p UserConfirmation="Are you sure? (type 'yes' to confirm) "
if "%UserConfirmation"=="yes" goto ProceedWithBuild
goto DoNotBuild
:ProceedWithBuild
echo Starting build
for /f "usebackq" %%A in (`""C:\Projects\ESME Deliverables\Utilities\VersionInfoIncrementer\bin\Release\VersionInfoIncrementer.exe" "C:\Projects\ESME Deliverables\Solutions\ESME Workbench\VersionInfo.txt""`) do set ESMEVersion=%%A
svn commit --message "Public release %ESMEVersion"
svn update
svn copy . "https://hrcsvn.bu.edu/svn/esme/src/bu/ESME Deliverables/tags/release builds/%ESMEVersion" --message "Public release %ESMEVersion"
call "BuildAndDeploy.bat" download  %ESMEVersion
goto End
:DoNotBuild
echo Build canceled
:End