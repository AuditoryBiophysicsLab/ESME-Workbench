﻿using System.Collections.Generic;
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

            ESMEBootstrapper.Model.Bootstrapper.ExecuteMsiMessage += ExecuteMsiMessage;
            ESMEBootstrapper.Model.Bootstrapper.ExecuteProgress += ApplyExecuteProgress;
            ESMEBootstrapper.Model.Bootstrapper.PlanBegin += PlanBegin;
            ESMEBootstrapper.Model.Bootstrapper.PlanPackageComplete += PlanPackageComplete;
            ESMEBootstrapper.Model.Bootstrapper.Progress += ApplyProgress;
            ESMEBootstrapper.Model.Bootstrapper.CacheAcquireProgress += CacheAcquireProgress;
            ESMEBootstrapper.Model.Bootstrapper.CacheComplete += CacheComplete;
        }

        public bool ProgressEnabled { get { return _root.State == InstallationState.Applying; } }

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

                if (ESMEBootstrapper.Model.Command.Display == Display.Embedded)
                    ESMEBootstrapper.Model.Engine.SendEmbeddedProgress(e.ProgressPercentage, Progress);

                e.Result = _root.Canceled ? Result.Cancel : Result.Ok;
            }
        }
    }
}