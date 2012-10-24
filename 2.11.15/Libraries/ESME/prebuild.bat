
cd %1
"C:\Projects\ESME Deliverables\Utilities\ProjectBuildInfo\bin\Release\ProjectBuildInfo.exe" -namespace ESME -class BuildInformation -svnversion . -output BuildInformation.cs
cd NewNEMO
"C:\Program Files\Microsoft SDKs\Windows\v7.0\Bin\x64\xsd.exe" NemoFile.xml
"C:\Program Files\Microsoft SDKs\Windows\v7.0\Bin\x64\xsd.exe" NemoFile.xsd /classes
