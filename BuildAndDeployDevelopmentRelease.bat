@echo off
for /f "usebackq" %%A in (`""C:\Projects\ESME Deliverables\Utilities\VersionInfoIncrementer\bin\Release\VersionInfoIncrementer.exe" "C:\Projects\ESME Deliverables\Solutions\ESME Workbench\VersionInfo.txt""`) do set ESMEVersion=%%A
rem svn commit --message "Development release %ESMEVersion%"
rem svn update
rem svn copy . "https://hrcsvn.bu.edu/svn/esme/src/bu/ESME Deliverables/tags/development builds/%ESMEVersion%" --message "Tag for development release %ESMEVersion%"
git commit -a -m "Development release %ESMEVersion%"
git tag -a %ESMEVersion% -m "Development release %ESMEVersion%"
git push
call "BuildAndDeploy.bat" development %ESMEVersion%
echo Successfully built development release %ESMEVersion%