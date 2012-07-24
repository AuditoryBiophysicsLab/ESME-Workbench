using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

namespace WixBootstrapper
{
    public class ProgressViewModel : PropertyNotifyBase
    {
        private readonly RootViewModel _root;
        private readonly Dictionary<string, int> _executingPackageOrderIndex;


        public ProgressViewModel(RootViewModel root)
        {
            _root = root;
            _executingPackageOrderIndex = new Dictionary<string, int>();

            _root.PropertyChanged += RootPropertyChanged;

            Bootstrapper.Model.Bootstrapper.ExecuteMsiMessage += ExecuteMsiMessage;
            Bootstrapper.Model.Bootstrapper.ExecuteProgress += ApplyExecuteProgress;
            Bootstrapper.Model.Bootstrapper.PlanBegin += PlanBegin;
            Bootstrapper.Model.Bootstrapper.PlanPackageComplete += PlanPackageComplete;
            Bootstrapper.Model.Bootstrapper.Progress += ApplyProgress;
            Bootstrapper.Model.Bootstrapper.CacheAcquireProgress += CacheAcquireProgress;
            Bootstrapper.Model.Bootstrapper.CacheComplete += CacheComplete;
            CacheMessage = "Acquisition progress";
            ExecuteMessage = "Installation progress";
            OverallMessage = "Overall progress";
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
                    case InstallationState.Applied:
                    case InstallationState.DetectedAbsent:
                    case InstallationState.DetectedPresent:
                    case InstallationState.DetectedNewer:
                    case InstallationState.Failed:
                    default:
                        return false;
                }
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

        private string _overallMessage;
        public string OverallMessage
        {
            get { return _overallMessage; }

            set
            {
                if (_overallMessage == value) return;
                _overallMessage = value;
                base.OnPropertyChanged("OverallMessage");
            }
        }

        void RootPropertyChanged(object sender, PropertyChangedEventArgs e) { if ("State" == e.PropertyName) base.OnPropertyChanged("ProgressEnabled"); }

        private void PlanBegin(object sender, PlanBeginEventArgs e) { lock (this) _executingPackageOrderIndex.Clear(); }

        private void PlanPackageComplete(object sender, PlanPackageCompleteEventArgs e)
        {
            if (ActionState.None == e.Execute) return;
            lock (this)
            {
                Debug.Assert(!_executingPackageOrderIndex.ContainsKey(e.PackageId));
                _executingPackageOrderIndex.Add(e.PackageId, _executingPackageOrderIndex.Count);
            }
        }

        private void ExecuteMsiMessage(object sender, ExecuteMsiMessageEventArgs e)
        {
            lock (this)
            {
                if (e.Data.Count == 2 && e.Data[0] == "1") ExecuteMessage = string.Format("Installing {0}", e.Data[1]);
                Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, string.Format("ExecuteMsiMessage"));
                Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, string.Format("    DisplayParameters: {0}", e.DisplayParameters));
                Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, string.Format("    Message: {0}", e.Message));
                Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, string.Format("    MessageType: {0}", e.MessageType));
                Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, string.Format("    PackageId: {0}", e.PackageId));
                for (var dataIndex = 0; dataIndex < e.Data.Count; dataIndex++)
                    Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, string.Format("    Data[{0}]: \"{1}\"", dataIndex, e.Data[dataIndex]));
                //OverallMessage = e.Message;
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
                CacheMessage = string.Format("Acquiring {0}", e.PackageOrContainerId);
                OverallProgress = (_cacheProgress + _executeProgress) / 2;
                e.Result = _root.Canceled ? Result.Cancel : Result.Ok;
            }
        }

        private void CacheComplete(object sender, CacheCompleteEventArgs e)
        {
            lock (this)
            {
                CacheMessage = string.Format("Package acquisition complete");
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
