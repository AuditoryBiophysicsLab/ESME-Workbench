rem @echo off
cd "%~dp0"
for /f "usebackq" %%A in (`""%~dp0\Utilities\VersionInfoIncrementer\bin\Release\VersionInfoIncrementer.exe" "%~dp0\Solutions\ESME Workbench\VersionInfo.txt""`) do set ESMEVersion=%%A
IF %ERRORLEVEL% NEQ 0 goto Failure

rem svn commit --message "Development release %ESMEVersion%"
rem svn update
rem svn copy . "https://hrcsvn.bu.edu/svn/esme/src/bu/ESME Deliverables/tags/development builds/%ESMEVersion%" --message "Tag for development release %ESMEVersion%"
git commit -a -m "Development release %ESMEVersion%"
IF %ERRORLEVEL% NEQ 0 goto Failure
git tag -a %ESMEVersion% -m "Development release %ESMEVersion%"
IF %ERRORLEVEL% NEQ 0 goto Failure
git push --tags
IF %ERRORLEVEL% NEQ 0 goto Failure
git push
IF %ERRORLEVEL% NEQ 0 goto Failure
call "BuildAndDeploy.bat" development %ESMEVersion%
IF %ERRORLEVEL% NEQ 0 goto Failure
echo Successfully built development release %ESMEVersion%
goto End
:Failure
echo Build of development release %ESMEVersion% FAILED
pause
