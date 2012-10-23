@for /f "tokens=*" %V in ('Utilities\VersionInfoIncrementer\bin\Debug\VersionInfoIncrementer.exe "C:\Projects\ESME Deliverables\Solutions\ESME Workbench\VersionInfo.txt"') do set ESMEVersion=%V
svn commit --message "Development release %ESMEVersion%"
call "BuildAndDeploy.bat" development
