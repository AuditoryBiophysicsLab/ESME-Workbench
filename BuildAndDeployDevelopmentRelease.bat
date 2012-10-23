@for /f "tokens=*" %V in ('Utilities\VersionInfoIncrementer\bin\Debug\VersionInfoIncrementer.exe "C:\Projects\ESME Deliverables\Solutions\ESME Workbench\VersionInfo.txt"') do set ESMEVersion=%V

call "BuildAndDeploy.bat" development
