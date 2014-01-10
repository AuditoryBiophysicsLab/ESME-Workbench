@echo off
cd "%~dp0"
echo This script will build and release the current version of ESME Workbench 
echo to the world.
echo.
echo Please be sure you really want to do this!
echo.
echo Have you updated the minor version number (if appropriate)?
echo.
set /p UserConfirmation="Are you sure? (type 'yes' to confirm) "
if "%UserConfirmation%"=="yes" goto ProceedWithBuild
goto DoNotBuild
:ProceedWithBuild
echo Starting build
for /f "usebackq" %%A in (`""%~dp0\Utilities\VersionInfoIncrementer\bin\Release\VersionInfoIncrementer.exe" "%~dp0\Solutions\ESME Workbench\VersionInfo.txt""`) do set ESMEVersion=%%A
IF %ERRORLEVEL% NEQ 0 goto Failure
rem svn commit --message "Public release %ESMEVersion%"
rem svn update
rem svn copy . "https://hrcsvn.bu.edu/svn/esme/src/bu/ESME Deliverables/tags/release builds/%ESMEVersion%" --message "Tag for public release %ESMEVersion%"
git commit -a -m "Public release %ESMEVersion%"
IF %ERRORLEVEL% NEQ 0 goto Failure
git tag -a %ESMEVersion% -m "Public release %ESMEVersion%"
IF %ERRORLEVEL% NEQ 0 goto Failure
git push --tags
IF %ERRORLEVEL% NEQ 0 goto Failure
git push
IF %ERRORLEVEL% NEQ 0 goto Failure
call "BuildAndDeploy.bat" download  %ESMEVersion%
IF %ERRORLEVEL% NEQ 0 goto Failure
goto Success
:DoNotBuild
echo Build canceled
goto Done
:Success
echo Successfully built public release %ESMEVersion%
goto Done
:Failure
echo Build of public release %ESMEVersion% FAILED
goto Done
:Done
pause
