for /f "usebackq" %%A in (`""C:\Projects\ESME Deliverables\Utilities\VersionInfoIncrementer\bin\Debug\VersionInfoIncrementer.exe" "C:\Projects\ESME Deliverables\Solutions\ESME Workbench\VersionInfo.txt""`) do set ESMEVersion=%%A
svn commit --message "Development release %ESMEVersion%"
svn copy . "https://hrcsvn.bu.edu/svn/esme/src/bu/ESME Deliverables/tags/development builds/%ESMEVersion%" --message "Development release %ESMEVersion%/"
call "BuildAndDeploy.bat" development
