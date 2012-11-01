if "%1"=="download" goto ParameterOK
if "%1"=="development" goto ParameterOK
goto ParameterRequired
:ParameterOK
call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"
msbuild "ESME WorkBench\ESME Workbench.csproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform=AnyCPU /t:Rebuild /v:minimal
msbuild "TransmissionLossViewer\TransmissionLossViewer.csproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform=AnyCPU /t:Build /v:minimal

msbuild "SimulationLogAnalysis\SimulationLogAnalysis.csproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform=AnyCPU /t:Build /v:minimal

msbuild "Installers\ManagedBootstrapper\Managed Bootstrapper.csproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform=AnyCPU /t:Rebuild /v:minimal

set DownloadURLRoot=http://esme.bu.edu/%1%

msbuild "Installers\MSI Packages\Plugins\Environmental Databases\InstallableNAVO\InstallableNAVOPlugin MSI.wixproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform="x64" /t:Rebuild /v:minimal
msbuild "Installers\MSI Packages\Plugins\Environmental Databases\InstallableNAVO\InstallableNAVOPlugin MSI.wixproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform="x86" /t:Rebuild /v:minimal
msbuild "Installers\MSI Packages\ESME Application MSI\ESME Application MSI.wixproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform="x64" /t:Rebuild /v:minimal
msbuild "Installers\MSI Packages\ESME Application MSI\ESME Application MSI.wixproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform="x86" /t:Rebuild /v:minimal

msbuild "Installers\EXE Packages\ESME Application\ESME Application.wixproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform="x64" /t:Rebuild /v:minimal /p:DefineConstants="DownloadURLRoot=%DownloadURLRoot%"
msbuild "Installers\EXE Packages\ESME Application\ESME Application.wixproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform="x86" /t:Rebuild /v:minimal /p:DefineConstants="DownloadURLRoot=%DownloadURLRoot%"
msbuild "Installers\EXE Packages\ESME Application and Databases\ESME Application and Databases.wixproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform="x64" /t:Rebuild /v:minimal /p:DefineConstants="DownloadURLRoot=%DownloadURLRoot%"
msbuild "Installers\EXE Packages\ESME Application and Databases\ESME Application and Databases.wixproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform="x86" /t:Rebuild /v:minimal /p:DefineConstants="DownloadURLRoot=%DownloadURLRoot%"
msbuild "Installers\EXE Packages\ESME Databases\ESME Databases.wixproj" /p:SolutionDir="C:\Projects\ESME Deliverables\Solutions\ESME Workbench\\";Configuration=Release;Platform="x86" /t:Rebuild /v:minimal /p:DefineConstants="DownloadURLRoot=%DownloadURLRoot%"

copy /y "Installers\MSI Packages\ESME Application MSI\msi\*.msi" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1%"
copy /y "Installers\MSI Packages\Plugins\Environmental Databases\InstallableNAVO\msi\*.msi" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1%"
copy /y "Installers\EXE Packages\ESME Application\msi\*.exe" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1%"
copy /y "Installers\EXE Packages\ESME Application and Databases\msi\*.exe" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1%"
copy /y "Installers\EXE Packages\ESME Databases\msi\*.exe" "\\earlab.bu.edu\c$\Inetpub\wwwroot\ESME\%1%"
goto End
:ParameterRequired
echo This script requires a parameter, which must be either 'development' or 'download'
:End
