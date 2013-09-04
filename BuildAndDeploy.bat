call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"

if "%1"=="download" goto ParameterOK
if "%1"=="development" goto ParameterOK
if "%2" NEQ "" goto ParameterOK
goto ParameterRequired
:ParameterOK

msbuild "ESME WorkBench\ESME Workbench.csproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform=AnyCPU /t:Rebuild /v:minimal
msbuild "TransmissionLossViewer\TransmissionLossViewer.csproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform=AnyCPU /t:Build /v:minimal

msbuild "SimulationLogAnalysis\SimulationLogAnalysis.csproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform=AnyCPU /t:Build /v:minimal

msbuild "Plugins\Environmental Data Sources\InstallableNAVO\InstallableNAVOPlugin.csproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform=AnyCPU /t:Rebuild /v:minimal

msbuild "Plugins\Transmission Loss Calculators\StandardTransmissionLossEngines\StandardTransmissionLossEngines.csproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform=AnyCPU /t:Rebuild /v:minimal

msbuild "Installers\ManagedBootstrapper\Managed Bootstrapper.csproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform=AnyCPU /t:Rebuild /v:minimal

set DownloadURLRoot=http://esme.bu.edu/%1
set ESMERoot=%~dp0
rem msbuild "Installers\MSI Packages\Plugins\Environmental Databases\InstallableNAVO\InstallableNAVOPlugin MSI.wixproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform="x64" /t:Rebuild /v:minimal

msbuild "Installers\MSI Packages\Plugins\Environmental Databases\InstallableNAVO\InstallableNAVOPlugin MSI.wixproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform="x64" /t:Rebuild
msbuild "Installers\MSI Packages\Plugins\Environmental Databases\InstallableNAVO\InstallableNAVOPlugin MSI.wixproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform="x86" /t:Rebuild
msbuild "Installers\MSI Packages\ESME Application MSI\ESME Application MSI.wixproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform="x64" /t:Rebuild
msbuild "Installers\MSI Packages\ESME Application MSI\ESME Application MSI.wixproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform="x86" /t:Rebuild

msbuild "Installers\EXE Packages\ESME Application\ESME Application.wixproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform="x64" /t:Rebuild /p:DefineConstants="DownloadURLRoot=%DownloadURLRoot%;ESMERoot=%ESMERoot%"
msbuild "Installers\EXE Packages\ESME Application\ESME Application.wixproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform="x86" /t:Rebuild /p:DefineConstants="DownloadURLRoot=%DownloadURLRoot%;ESMERoot=%ESMERoot%"
msbuild "Installers\EXE Packages\ESME Application and Databases\ESME Application and Databases.wixproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform="x64" /t:Rebuild /p:DefineConstants="DownloadURLRoot=%DownloadURLRoot%;ESMERoot=%ESMERoot%"
msbuild "Installers\EXE Packages\ESME Application and Databases\ESME Application and Databases.wixproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform="x86" /t:Rebuild /p:DefineConstants="DownloadURLRoot=%DownloadURLRoot%;ESMERoot=%ESMERoot%"
msbuild "Installers\EXE Packages\ESME Databases\ESME Databases.wixproj" /p:SolutionDir="%~dp0\Solutions\ESME Workbench\\";Configuration=Release;Platform="x86" /t:Rebuild /p:DefineConstants="DownloadURLRoot=%DownloadURLRoot%;ESMERoot=%ESMERoot%"

goto End
copy /y "Installers\MSI Packages\ESME Application MSI\msi\*.msi" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1"
copy /y "Installers\MSI Packages\Plugins\Environmental Databases\InstallableNAVO\msi\*.msi" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1"
copy /y "Installers\EXE Packages\ESME Application\msi\*.exe" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1"
copy /y "Installers\EXE Packages\ESME Application and Databases\msi\*.exe" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1"
copy /y "Installers\EXE Packages\ESME Databases\msi\*.exe" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1"

mkdir "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1\versions\%2"

copy /y "Installers\MSI Packages\ESME Application MSI\msi\*.msi" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1\versions\%2"
copy /y "Installers\MSI Packages\Plugins\Environmental Databases\InstallableNAVO\msi\*.msi" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1\versions\%2"
copy /y "Installers\EXE Packages\ESME Application\msi\*.exe" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1\versions\%2"
copy /y "Installers\EXE Packages\ESME Application and Databases\msi\*.exe" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1\versions\%2"
copy /y "Installers\EXE Packages\ESME Databases\msi\*.exe" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1\versions\%2"

goto End
:ParameterRequired
echo This script requires two parameters, the first of which must be either 'development' or 'download'
echo the second parameter must be the current version number
:End
