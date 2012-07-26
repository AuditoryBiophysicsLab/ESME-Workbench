using System.Collections.Generic;
using System.ComponentModel;
//using System.Diagnostics;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

namespace WixBootstrapper
{
    public class ProgressViewModel : PropertyNotifyBase
    {
        private readonly RootViewModel _root;
        private readonly Dictionary<string, int> _executingPackageOrderIndex;
        readonly BootstrapperApplication _ba;

        public ProgressViewModel(RootViewModel root)
        {
            _root = root;
            _executingPackageOrderIndex = new Dictionary<string, int>();

            _root.PropertyChanged += RootPropertyChanged;

            _ba = Bootstrapper.Model.Bootstrapper;
            _ba.CacheAcquireBegin += (s, e) => CacheMessage = string.Format("{0}ing {1}", e.Operation, e.PackageOrContainerId);
            _ba.ExecuteMsiMessage += ExecuteMsiMessage;
            _ba.ExecuteProgress += ApplyExecuteProgress;
            _ba.PlanBegin += PlanBegin;
            _ba.PlanPackageComplete += PlanPackageComplete;
            _ba.Progress += ApplyProgress;
            _ba.CacheAcquireProgress += CacheAcquireProgress;
            _ba.CacheComplete += CacheComplete;
            CacheMessage = "Acquisition progress";
#if false
            #region Verbose logging for many bootstrapper events

            // Fired when the engine has begun installing the bundle
            _ba.ApplyBegin += (s, e) => _ba.Engine.Log(LogLevel.Verbose, "ApplyBegin");

            // Fired when the engine has completed installing the bundle
            _ba.ApplyComplete += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "ApplyComplete");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Restart: {0}", e.Restart));
            };

            // Fired when the engine has begun acquiring the installation sources
            _ba.CacheAcquireBegin += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "CacheAcquireBegin");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Operation: {0}", e.Operation));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageOrContainerId: {0}", e.PackageOrContainerId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PayloadId: {0}", e.PayloadId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Source: {0}", e.Source));
            };

            // Fired when the engine has completed the acquisition of the installation sources
            _ba.CacheAcquireComplete += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "CacheAcquireComplete");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageOrContainerId: {0}", e.PackageOrContainerId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PayloadId: {0}", e.PayloadId));
            };

            // Fired when the engine has progress aquiring the installation sources
            _ba.CacheAcquireProgress += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "CacheAcquireProgress");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    OverallPercentage: {0}", e.OverallPercentage));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageOrContainerId: {0}", e.PackageOrContainerId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PayloadId: {0}", e.PayloadId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Progress: {0}", e.Progress));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Total: {0}", e.Total));
            };

            // Fired when the engine has begun caching the installation sources
            _ba.CacheBegin += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "CacheBegin");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Result: {0}", e.Result));
            };

            // Fired when the engine has cached the installation sources
            _ba.CacheComplete += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "CacheComplete");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Status: {0}", e.Status));
            };

            // Fired when the engine has begun caching a specific package
            _ba.CachePackageBegin += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "CachePackageBegin");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    CachePayloads: {0}", e.CachePayloads));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageCacheSize: {0}", e.PackageCacheSize));
            };

            // Fired when the engine has completed caching a specific package
            _ba.CachePackageComplete += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "CachePackageComplete");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
            };

            // Fired when the engine begins the verification of the acquired installation sources
            _ba.CacheVerifyBegin += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "CacheVerifyBegin");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PayloadId: {0}", e.PayloadId));
            };

            // Fired when the engine has completed the verification of the acquired installation sources
            _ba.CacheVerifyComplete += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "CacheVerifyComplete");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PayloadId: {0}", e.PayloadId));
            };

            // Fired when the overall detection phase has begun
            _ba.DetectBegin += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "DetectBegin");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageCount: {0}", e.PackageCount));
            };

            // Fired when the detection phase has completed
            _ba.DetectComplete += (s, e) => _ba.Engine.Log(LogLevel.Verbose, "DetectComplete");

            // Fired when a feature in an MSI package has been detected
            _ba.DetectMsiFeature += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "DetectMsiFeature");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    FeatureId: {0}", e.FeatureId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    State: {0}", e.State));
            };

            // Fired when the detection for a specific package has begun
            _ba.DetectPackageBegin += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "DetectPackageBegin");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
            };

            // Fired when the detection for a specific package has completed
            _ba.DetectPackageComplete += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "DetectPackageComplete");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    State: {0}", e.State));
            };

            // Fired when the detection for a prior bundle has begun
            _ba.DetectPriorBundle += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "DetectPriorBundle");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    BundleId: {0}", e.BundleId));
            };

            // Fired when a related bundle has been detected for a bundle
            _ba.DetectRelatedBundle += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "DetectRelatedBundle");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    BundleTag: {0}", e.BundleTag));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Operation: {0}", e.Operation));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PerMachine: {0}", e.PerMachine));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    ProductCode: {0}", e.ProductCode));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Version: {0}", e.Version));
            };

            // Fired when a related MSI package has been detected for a package
            _ba.DetectRelatedMsiPackage += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "DetectRelatedMsiPackage");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Operation: {0}", e.Operation));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PerMachine: {0}", e.PerMachine));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    ProductCode: {0}", e.ProductCode));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Version: {0}", e.Version));
            };

            // Fired when an MSP package detects a target MSI package has been detected
            _ba.DetectTargetMsiPackage += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "DetectTargetMsiPackage");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    ProductCode: {0}", e.ProductCode));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    State: {0}", e.State));
            };

            // Fired when the engine is about to start the elevated process
            _ba.Elevate += (s, e) => _ba.Engine.Log(LogLevel.Verbose, "Elevate");

            // Fired when the engine has encountered an error
            _ba.Error += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "Error");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Data: {0}", e.Data));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    ErrorCode: {0}", e.ErrorCode));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    ErrorMessage: {0}", e.ErrorMessage));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    ErrorType: {0}", e.ErrorType));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    UIHint: {0}", e.UIHint));
            };

            // Fired when the engine has begun installing packages
            _ba.ExecuteBegin += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "ExecuteBegin");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageCount: {0}", e.PackageCount));
            };

            // Fired when the engine has completed installing packages
            _ba.ExecuteComplete += (s, e) => _ba.Engine.Log(LogLevel.Verbose, "ExecuteComplete");

            // Fired when the Windows Installer sends a files in use installation message
            _ba.ExecuteFilesInUse += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "ExecuteFilesInUse");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                for (var dataIndex = 0; dataIndex < e.Files.Count; dataIndex++)
                    _ba.Engine.Log(LogLevel.Verbose, string.Format("    File[{0}]: \"{1}\"", dataIndex, e.Files[dataIndex]));
            };

            // Fired when the Windows Installer sends an installation message
            _ba.ExecuteMsiMessage += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "ExecuteMsiMessage");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    DisplayParameters: {0}", e.DisplayParameters));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Message: {0}", e.Message));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    MessageType: {0}", e.MessageType));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                for (var dataIndex = 0; dataIndex < e.Data.Count; dataIndex++)
                    _ba.Engine.Log(LogLevel.Verbose, string.Format("    Data[{0}]: \"{1}\"", dataIndex, e.Data[dataIndex]));
            };

            // Fired when the engine has begun installing a specific package
            _ba.ExecutePackageBegin += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "ExecutePackageBegin");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    ShouldExecute: {0}", e.ShouldExecute));
            };

            // Fired when the engine has completed installing a specific package
            _ba.ExecutePackageComplete += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "ExecutePackageComplete");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Restart: {0}", e.Restart));
            };

            // Fired when the engine executes one or more patches targeting a product
            _ba.ExecutePatchTarget += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "ExecutePatchTarget");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    TargetProductCode: {0}", e.TargetProductCode));
            };

            // Fired when the engine executes one or more patches targeting a product
            _ba.ExecuteProgress += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "ExecuteProgress");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    OverallPercentage: {0}", e.OverallPercentage));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    ProgressPercentage: {0}", e.ProgressPercentage));
            };

            // Fired when the engine has begun planning the installation
            _ba.PlanBegin += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "PlanBegin");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageCount: {0}", e.PackageCount));
            };

            // Fired when the engine has completed planning the installation
            _ba.PlanComplete += (s, e) => _ba.Engine.Log(LogLevel.Verbose, "PlanComplete");

            // Fired when the engine is about to plan a feature in an MSI package
            _ba.PlanMsiFeature += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "PlanMsiFeature");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    FeatureId: {0}", e.FeatureId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    State: {0}", e.State));
            };

            // Fired when the engine has begun planning the installation of a specific package
            _ba.PlanPackageBegin += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "PlanPackageBegin");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    State: {0}", e.State));
            };

            // Fired when the engine has begun planning the installation of a specific package
            _ba.PlanPackageComplete += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "PlanPackageComplete");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Execute: {0}", e.Execute));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Requested: {0}", e.Requested));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Rollback: {0}", e.Rollback));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    State: {0}", e.State));
            };

            // Fired when the engine has begun planning for a related bundle
            _ba.PlanRelatedBundle += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "PlanRelatedBundle");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    BundleId: {0}", e.BundleId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    State: {0}", e.State));
            };

            // Fired when the engine is about to plan the target MSI of an MSP package
            _ba.PlanTargetMsiPackage += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "PlanTargetMsiPackage");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    ProductCode: {0}", e.ProductCode));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    State: {0}", e.State));
            };

            // Fired when the engine has changed progress for the bundle installation
            _ba.Progress += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "Progress");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    OverallPercentage: {0}", e.OverallPercentage));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    ProgressPercentage: {0}", e.ProgressPercentage));
            };

            // Fired when the engine has begun registering the location and visibility of the bundle
            _ba.RegisterBegin += (s, e) => _ba.Engine.Log(LogLevel.Verbose, "RegisterBegin");

            // Fired when the engine has begun completed the location and visibility of the bundle
            _ba.RegisterComplete += (s, e) => _ba.Engine.Log(LogLevel.Verbose, "RegisterComplete");

            // Fired by the engine to allow the user experience to change the source using Engine.SetLocalSource or Engine.SetDownloadSource
            _ba.ResolveSource += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "ResolveSource");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    DownloadSource: {0}", e.DownloadSource));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    LocalSource: {0}", e.LocalSource));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PackageOrContainerId: {0}", e.PackageOrContainerId));
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    PayloadId: {0}", e.PayloadId));
            };

            // Fired by the engine to request a restart now or inform the user a manual restart is required later
            _ba.RestartRequired += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "RestartRequired");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Restart: {0}", e.Restart));
            };

            // Fired when the engine is shutting down the bootstrapper application
            _ba.Shutdown += (s, e) => _ba.Engine.Log(LogLevel.Verbose, "Shutdown");

            // Fired when the engine is starting up the bootstrapper application
            _ba.Startup += (s, e) => _ba.Engine.Log(LogLevel.Verbose, "Startup");

            // Fired when the system is shutting down or user is logging off
            _ba.SystemShutdown += (s, e) =>
            {
                _ba.Engine.Log(LogLevel.Verbose, "SystemShutdown");
                _ba.Engine.Log(LogLevel.Verbose, string.Format("    Reasons: {0}", e.Reasons));
            };

            // Fired when the engine has begun removing the registration for the location and visibility of the bundle
            _ba.UnregisterBegin += (s, e) => _ba.Engine.Log(LogLevel.Verbose, "UnregisterBegin");

            // Fired when the engine has completed removing the registration for the location and visibility of the bundle
            _ba.UnregisterComplete += (s, e) => _ba.Engine.Log(LogLevel.Verbose, "UnregisterComplete");

            #endregion
#endif
        }

        public bool ProgressEnabled
        {
            get
            {
                switch (_root.State)
                {
                    case InstallationState.Initializing:
                    case InstallationState.Applying:
                        return true;
                    //case InstallationState.Applied:
                    //case InstallationState.DetectedAbsent:
                    //case InstallationState.DetectedPresent:
                    //case InstallationState.DetectedNewer:
                    //case InstallationState.Failed:
                    default:
                        return false;
                }
            }
        }

        private bool _isInstall;
        public bool IsInstall
        {
            get { return _isInstall; }

            set
            {
                if (_isInstall == value) return;
                _isInstall = value;
                base.OnPropertyChanged("IsInstall");
                ExecuteMessage = "Install progress";
                CurrentAction = "Installing";
            }
        }

        private bool _isRepair;
        public bool IsRepair
        {
            get { return _isRepair; }

            set
            {
                if (_isRepair == value) return;
                _isRepair = value;
                base.OnPropertyChanged("IsRepair");
                ExecuteMessage = "Repair progress";
                CurrentAction = "Repairing";
            }
        }

        private bool _isUninstall;
        public bool IsUninstall
        {
            get { return _isUninstall; }

            set
            {
                if (_isUninstall == value) return;
                _isUninstall = value;
                base.OnPropertyChanged("IsUninstall");
                ExecuteMessage = "Uninstall progress";
                CurrentAction = "Uninstalling";
            }
        }

        private int _cacheProgress;
        public int CacheProgress
        {
            get { return _cacheProgress; }

            set
            {
                if (_cacheProgress == value) return;
                _cacheProgress = value;
                base.OnPropertyChanged("CacheProgress");
            }
        }

        private string _currentAction;
        public string CurrentAction
        {
            get { return _currentAction; }

            set
            {
                if (_currentAction == value) return;
                _currentAction = value;
                base.OnPropertyChanged("CurrentAction");
            }
        }

        private string _cacheMessage;
        public string CacheMessage
        {
            get { return _cacheMessage; }

            set
            {
                if (_cacheMessage == value) return;
                _cacheMessage = value;
                base.OnPropertyChanged("CacheMessage");
            }
        }

        private int _executeProgress;
        public int ExecuteProgress
        {
            get { return _executeProgress; }

            set
            {
                if (_executeProgress == value) return;
                _executeProgress = value;
                base.OnPropertyChanged("ExecuteProgress");
            }
        }

        private string _executeMessage;
        public string ExecuteMessage
        {
            get { return _executeMessage; }

            set
            {
                if (_executeMessage == value) return;
                _executeMessage = value;
                base.OnPropertyChanged("ExecuteMessage");
            }
        }


        private int _overallProgress;
        public int OverallProgress
        {
            get { return _overallProgress; }

            set
            {
                if (_overallProgress == value) return;
                _overallProgress = value;
                base.OnPropertyChanged("OverallProgress");
            }
        }

        void RootPropertyChanged(object sender, PropertyChangedEventArgs e) { if ("State" == e.PropertyName) base.OnPropertyChanged("ProgressEnabled"); }

        private void PlanBegin(object sender, PlanBeginEventArgs e)
        {
            lock (this) _executingPackageOrderIndex.Clear();
        }

        private void PlanPackageComplete(object sender, PlanPackageCompleteEventArgs e)
        {
            if (ActionState.None == e.Execute) return;
            lock (this)
            {
                //Debug.Assert(!_executingPackageOrderIndex.ContainsKey(e.PackageId));
                _executingPackageOrderIndex.Add(e.PackageId, _executingPackageOrderIndex.Count);
            }
        }

        private void ExecuteMsiMessage(object sender, ExecuteMsiMessageEventArgs e)
        {
            lock (this)
            {
                if (e.Data.Count == 2 && e.Data[0] == "1") ExecuteMessage = string.Format("{0} {1}", CurrentAction, e.Data[1]);
                e.Result = _root.Canceled ? Result.Cancel : Result.Ok;
            }
        }

        private void ApplyProgress(object sender, ProgressEventArgs e)
        {
            lock (this) e.Result = _root.Canceled ? Result.Cancel : Result.Ok;
        }

        private void CacheAcquireProgress(object sender, CacheAcquireProgressEventArgs e)
        {
            lock (this)
            {
                CacheProgress = e.OverallPercentage;
                OverallProgress = (_cacheProgress + _executeProgress) / 2;
                e.Result = _root.Canceled ? Result.Cancel : Result.Ok;
            }
        }

        private void CacheComplete(object sender, CacheCompleteEventArgs e)
        {
            lock (this)
            {
                if (IsInstall) CacheMessage = string.Format("All packages acquired");
                CacheProgress = 100;
                OverallProgress = (_cacheProgress + _executeProgress) / 2;
            }
        }

        private void ApplyExecuteProgress(object sender, ExecuteProgressEventArgs e)
        {
            lock (this)
            {
                ExecuteProgress = e.OverallPercentage;
                OverallProgress = (CacheProgress + ExecuteProgress) / 2;

                if (Bootstrapper.Model.Command.Display == Display.Embedded)
                    Bootstrapper.Model.Engine.SendEmbeddedProgress(e.ProgressPercentage, OverallProgress);

                e.Result = _root.Canceled ? Result.Cancel : Result.Ok;
            }
        }
    }
}
