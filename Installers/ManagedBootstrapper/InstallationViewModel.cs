//-------------------------------------------------------------------------------------------------
// <copyright file="InstallationViewModel.cs" company="Microsoft">
// Copyright (c) Microsoft Corporation. All rights reserved.
//    
//    The use and distribution terms for this software are covered by the
//    Common Public License 1.0 (http://opensource.org/licenses/cpl1.0.php)
//    which can be found in the file CPL.TXT at the root of this distribution.
//    By using this software in any fashion, you are agreeing to be bound by
//    the terms of this license.
//    
//    You must not remove this notice, or any other, from this software.
// </copyright>
// 
// <summary>
// The model of the installation view.
// </summary>
//-------------------------------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Windows;
using System.Windows.Input;
using System.Windows.Interop;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;
using ErrorEventArgs = Microsoft.Tools.WindowsInstallerXml.Bootstrapper.ErrorEventArgs;

namespace WixBootstrapper
{
    /// <summary>
    /// The states of the installation view model.
    /// </summary>
    public enum InstallationState
    {
        Initializing,
        DetectedAbsent,
        DetectedPresent,
        DetectedNewer,
        Applying,
        Applied,
        Failed,
    }

    /// <summary>
    /// The model of the installation view in Bootstrapper.
    /// </summary>
    public class InstallationViewModel : PropertyNotifyBase
    {
        private readonly RootViewModel _root;

        private readonly Dictionary<string, int> _downloadRetries;
        private bool _downgrade;
        const string ESMEHomePageUrl = "http://esme.bu.edu/";
        const string ESMENewsUrl = "http://esme.bu.edu/";

        private bool _planAttempted;
        private LaunchAction _plannedAction;
        private IntPtr _hwnd;

        private ICommand _licenseCommand;
        private ICommand _launchHomePageCommand;
        private ICommand _launchNewsCommand;
        private ICommand _installCommand;
        private ICommand _repairCommand;
        private ICommand _uninstallCommand;
        private ICommand _tryAgainCommand;

        private string _message;

        /// <summary>
        /// Creates a new model of the installation view.
        /// </summary>
        public InstallationViewModel(RootViewModel root)
        {
            Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, "Entering InstallationViewModel constructor");
            _root = root;
            _downloadRetries = new Dictionary<string, int>();

            _root.PropertyChanged += RootPropertyChanged;

            Bootstrapper.Model.Bootstrapper.DetectBegin += DetectBegin;
            Bootstrapper.Model.Bootstrapper.DetectRelatedBundle += DetectedRelatedBundle;
            Bootstrapper.Model.Bootstrapper.DetectPackageComplete += DetectedPackage;
            Bootstrapper.Model.Bootstrapper.DetectComplete += DetectComplete;
            Bootstrapper.Model.Bootstrapper.PlanPackageBegin += PlanPackageBegin;
            Bootstrapper.Model.Bootstrapper.PlanComplete += PlanComplete;
            Bootstrapper.Model.Bootstrapper.ApplyBegin += ApplyBegin;
            Bootstrapper.Model.Bootstrapper.Error += ExecuteError;
            Bootstrapper.Model.Bootstrapper.ResolveSource += ResolveSource;
            Bootstrapper.Model.Bootstrapper.ApplyComplete += ApplyComplete;
            Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, "Leaving InstallationViewModel constructor");
        }

        void RootPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            if ("State" != e.PropertyName) return;
            base.OnPropertyChanged("Title");
            base.OnPropertyChanged("CompleteEnabled");
            base.OnPropertyChanged("ExitEnabled");
            base.OnPropertyChanged("RepairEnabled");
            base.OnPropertyChanged("InstallEnabled");
            base.OnPropertyChanged("UninstallEnabled");
        }

        /// <summary>
        /// Gets the title for the application.
        /// </summary>
        public string Version { get { return String.Concat("v", Bootstrapper.Model.Version.ToString()); } }

        public string Message
        {
            get { return _message; }
            set
            {
                if (_message == value) return;
                _message = value;
                base.OnPropertyChanged("OverallMessage");
            }
        }

        /// <summary>
        /// Gets and sets whether the view model considers this install to be a downgrade.
        /// </summary>
        public bool Downgrade
        {
            get { return _downgrade; }
            set
            {
                if (_downgrade == value) return;
                _downgrade = value;
                base.OnPropertyChanged("Downgrade");
            }
        }

        public ICommand LaunchHomePageCommand
        {
            get 
            {
                return _launchHomePageCommand ?? (_launchHomePageCommand = new RelayCommand(param => Bootstrapper.LaunchUrl(ESMEHomePageUrl), param => true));
            }
        }

        public ICommand LaunchNewsCommand
        {
            get 
            {
                return _launchNewsCommand ?? (_launchNewsCommand = new RelayCommand(param => Bootstrapper.LaunchUrl(ESMENewsUrl), param => true));
            }
        }

        public ICommand LicenseCommand
        {
            get { return _licenseCommand ?? (_licenseCommand = new RelayCommand(param => LaunchLicense(), param => true)); }
        }

        public bool LicenseEnabled
        {
            get { return LicenseCommand.CanExecute(this); }
        }

        public ICommand CloseCommand
        {
            get { return _root.CloseCommand; }
        }

        public bool ExitEnabled
        {
            get { return _root.State != InstallationState.Applying; }
        }

        public ICommand InstallCommand
        {
            get 
            {
                return _installCommand ?? (_installCommand = new RelayCommand(param => Plan(LaunchAction.Install), param => _root.State == InstallationState.DetectedAbsent));
            }
        }

        public bool OptionsEnabled
        {
            get { return InstallEnabled || RepairEnabled || UninstallEnabled; }
        }

        public bool InstallEnabled
        {
            get { return InstallCommand.CanExecute(this); }
        }

        public ICommand RepairCommand
        {
            get 
            {
                return _repairCommand ?? (_repairCommand = new RelayCommand(param => Plan(LaunchAction.Repair), param => _root.State == InstallationState.DetectedPresent));
            }
        }

        public bool RepairEnabled
        {
            get { return RepairCommand.CanExecute(this); }
        }

        public bool CompleteEnabled
        {
            get { return _root.State == InstallationState.Applied; }
        }

        public ICommand UninstallCommand
        {
            get 
            {
                return _uninstallCommand ?? (_uninstallCommand = new RelayCommand(param => Plan(LaunchAction.Uninstall), param => _root.State == InstallationState.DetectedPresent));
            }
        }

        public bool UninstallEnabled
        {
            get { return UninstallCommand.CanExecute(this); }
        }

        public ICommand TryAgainCommand
        {
            get 
            {
                return _tryAgainCommand ?? (_tryAgainCommand = new RelayCommand(param => Plan(_plannedAction), param => _root.State == InstallationState.Failed));
            }
        }

        public bool TryAgainEnabled
        {
            get { return TryAgainCommand.CanExecute(this); }
        }

        public string Title
        {
            get
            {
                switch (_root.State)
                {
                    case InstallationState.Initializing:
                        return "Initializing...";

                    case InstallationState.DetectedPresent:
                        return "Installed";

                    case InstallationState.DetectedNewer:
                        return "Newer version installed";

                    case InstallationState.DetectedAbsent:
                        return "Not installed";

                    case InstallationState.Applying:
                        switch (_plannedAction)
                        {
                            case LaunchAction.Install:
                                return "Installing...";

                            case LaunchAction.Repair:
                                return "Repairing...";

                            case LaunchAction.Uninstall:
                                return "Uninstalling...";

                            default:
                                return "Unexpected action state";
                        }

                    case InstallationState.Applied:
                        switch (_plannedAction)
                        {
                            case LaunchAction.Install:
                                return "Successfully installed";

                            case LaunchAction.Repair:
                                return "Successfully repaired";

                            case LaunchAction.Uninstall:
                                return "Successfully uninstalled";

                            default:
                                return "Unexpected action state";
                        }

                    case InstallationState.Failed:
                        if (_root.Canceled)
                        {
                            return "Canceled";
                        }
                        if (_planAttempted)
                        {
                            switch (_plannedAction)
                            {
                                case LaunchAction.Install:
                                    return "Failed to install";

                                case LaunchAction.Repair:
                                    return "Failed to repair";

                                case LaunchAction.Uninstall:
                                    return "Failed to uninstall";

                                default:
                                    return "Unexpected action state";
                            }
                        }
                        return "Unexpected failure";

                    default:
                        return "Unknown view model state";
                }
            }
        }

        /// <summary>
        /// Causes the installation view to re-detect machine state.
        /// </summary>
        public void Refresh()
        {
            Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, "Entering InstallationViewModel.Refresh()");
            // TODO: verify that the engine is in a state that will allow it to do Detect().

            _root.Canceled = false;
            Bootstrapper.Model.Engine.Detect();
            Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, "Exiting InstallationViewModel.Refresh()");
        }

        /// <summary>
        /// Launches the license in the default viewer.
        /// </summary>
        private static void LaunchLicense()
        {
            var folder = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            Bootstrapper.LaunchUrl(Path.Combine(folder, "License.htm"));
        }

        /// <summary>
        /// Starts planning the appropriate action.
        /// </summary>
        /// <param name="action">Action to plan.</param>
        private void Plan(LaunchAction action)
        {
            _planAttempted = true;
            _plannedAction = action;
            _hwnd = (Bootstrapper.View == null) ? IntPtr.Zero : new WindowInteropHelper(Bootstrapper.View).Handle;

            _root.Canceled = false;
            Bootstrapper.Model.Engine.Plan(_plannedAction);
        }

        private void DetectBegin(object sender, DetectBeginEventArgs e)
        {
            _root.State = InstallationState.Initializing;
            _planAttempted = false;
        }

        private void DetectedRelatedBundle(object sender, DetectRelatedBundleEventArgs e)
        {
            if (e.Operation == RelatedOperation.Downgrade)
            {
                Downgrade = true;
            }
        }

        private void DetectedPackage(object sender, DetectPackageCompleteEventArgs e)
        {
            Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, "Entering InstallationViewModel.DetectedPackage()");
            Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, string.Format("  e.PackageID = '{0}' is {1}", e.PackageId, (e.State == PackageState.Present) ? "Present" : "Absent"));
            //if (e.PackageId.Equals("Wix", StringComparison.Ordinal))
            _root.State = (e.State == PackageState.Present) ? InstallationState.DetectedPresent : InstallationState.DetectedAbsent;
            Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, "Leaving InstallationViewModel.DetectedPackage()");
        }

        private void DetectComplete(object sender, DetectCompleteEventArgs e)
        {
            if (LaunchAction.Uninstall == Bootstrapper.Model.Command.Action)
            {
                Bootstrapper.Model.Engine.Log(LogLevel.Verbose, "Invoking automatic plan for uninstall");
                Bootstrapper.Dispatcher.Invoke((Action)(() => Plan(LaunchAction.Uninstall)));
            }
            else if (Hresult.Succeeded(e.Status))
            {
                if (Downgrade)
                {
                    // TODO: What behavior do we want for downgrade?
                    _root.State = InstallationState.DetectedNewer;
                }

                // If we're not waiting for the user to click install, dispatch plan with the default action.
                if (Bootstrapper.Model.Command.Display != Display.Full)
                {
                    Bootstrapper.Model.Engine.Log(LogLevel.Verbose, "Invoking automatic plan for non-interactive mode.");
                    Bootstrapper.Dispatcher.Invoke((Action)(() => Plan(Bootstrapper.Model.Command.Action)));
                }
            }
            else
            {
                _root.State = InstallationState.Failed;
            }
        }

        private static void PlanPackageBegin(object sender, PlanPackageBeginEventArgs e)
        {
            if (e.PackageId.Equals(Bootstrapper.Model.Engine.StringVariables["MbaNetfxPackageId"], StringComparison.Ordinal))
                e.State = RequestState.None;
        }

        private void PlanComplete(object sender, PlanCompleteEventArgs e)
        {
            if (Hresult.Succeeded(e.Status))
            {
                _root.PreApplyState = _root.State;
                _root.State = InstallationState.Applying;
                Bootstrapper.Model.Engine.Apply(_hwnd);
            }
            else
                _root.State = InstallationState.Failed;
        }

        private void ApplyBegin(object sender, ApplyBeginEventArgs e)
        {
            _downloadRetries.Clear();
        }

        private void ExecuteError(object sender, ErrorEventArgs e)
        {
            lock (this)
            {
                if (!_root.Canceled)
                {
                    // If the error is a cancel coming from the engine during apply we want to go back to the preapply state.
                    if (InstallationState.Applying == _root.State && (int)Error.UserCancelled == e.ErrorCode)
                        _root.State = _root.PreApplyState;
                    else
                    {
                        Message = e.ErrorMessage;

                        Bootstrapper.View.Dispatcher.Invoke((Action)(() => MessageBox.Show(Bootstrapper.View, e.ErrorMessage,
                                                                                               "ESME Workbench", MessageBoxButton.OK, MessageBoxImage.Error)));
                    }
                }

                e.Result = _root.Canceled ? Result.Cancel : Result.Ok;
            }
        }

        private void ResolveSource(object sender, ResolveSourceEventArgs e)
        {
            Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, string.Format("Entering ResolveSource. DownloadSource: {0} LocalSource: {1}", e.DownloadSource, e.LocalSource));
            int retries;

            _downloadRetries.TryGetValue(e.PackageOrContainerId, out retries);
            _downloadRetries[e.PackageOrContainerId] = retries + 1;
            e.Result = retries < 3 && !String.IsNullOrEmpty(e.DownloadSource) ? Result.Download : Result.Ok;
            if (e.Result == Result.Download) Message = "Downloading " + e.PackageOrContainerId;
            Bootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, string.Format("Leaving ResolveSource. DownloadSource: {0} LocalSource: {1}", e.DownloadSource, e.LocalSource));
        }

        private void ApplyComplete(object sender, ApplyCompleteEventArgs e)
        {
            // If we're not in Full UI mode, we need to alert the dispatcher to stop and close the window for passive.
            if (Display.Full != Bootstrapper.Model.Command.Display)
            {
                // If its passive, send a message to the window to close.
                if (Display.Passive == Bootstrapper.Model.Command.Display)
                {
                    Bootstrapper.Model.Engine.Log(LogLevel.Verbose, "Automatically closing the window for non-interactive install");
                    Bootstrapper.Dispatcher.BeginInvoke((Action)(() => Bootstrapper.View.Close()));
                }
                else
                    Bootstrapper.Dispatcher.InvokeShutdown();
            }

            // Set the state to applied or failed unless the state has already been set back to the preapply state
            // which means we need to show the UI as it was before the apply started.
            if (_root.State != _root.PreApplyState)
                _root.State = Hresult.Succeeded(e.Status) ? InstallationState.Applied : InstallationState.Failed;
        }
    }
}
