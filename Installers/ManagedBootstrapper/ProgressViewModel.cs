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

        private int _progress;
        private int _cacheProgress;
        private int _executeProgress;
        private string _message;

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

        public int Progress
        {
            get { return _progress; }

            set 
            {
                if (_progress == value) return;
                _progress = value;
                base.OnPropertyChanged("Progress");
            }
        }

        public string Message
        {
            get { return _message; }

            set
            {
                if (_message == value) return;
                _message = value;
                base.OnPropertyChanged("Message");
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
                Message = e.Message;
                e.Result = _root.Canceled ? Result.Cancel : Result.Ok;
            }
        }

        private void ApplyProgress(object sender, ProgressEventArgs e) { lock (this) e.Result = _root.Canceled ? Result.Cancel : Result.Ok; }

        private void CacheAcquireProgress(object sender, CacheAcquireProgressEventArgs e)
        {
            lock (this)
            {
                _cacheProgress = e.OverallPercentage;
                Progress = (_cacheProgress + _executeProgress) / 2;
                e.Result = _root.Canceled ? Result.Cancel : Result.Ok;
            }
        }

        private void CacheComplete(object sender, CacheCompleteEventArgs e)
        {
            lock (this)
            {
                _cacheProgress = 100;
                Progress = (_cacheProgress + _executeProgress) / 2;
            }
        }

        private void ApplyExecuteProgress(object sender, ExecuteProgressEventArgs e)
        {
            lock (this)
            {
                _executeProgress = e.OverallPercentage;
                Progress = (_cacheProgress + _executeProgress) / 2;

                if (Bootstrapper.Model.Command.Display == Display.Embedded)
                    Bootstrapper.Model.Engine.SendEmbeddedProgress(e.ProgressPercentage, Progress);

                e.Result = _root.Canceled ? Result.Cancel : Result.Ok;
            }
        }
    }
}
