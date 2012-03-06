﻿//-------------------------------------------------------------------------------------------------
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

namespace ManagedBootstrapper
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
    /// The model of the installation view in WixBA.
    /// </summary>
    public class InstallationViewModel : PropertyNotifyBase
    {
        private readonly RootViewModel _root;

        private readonly Dictionary<string, int> _downloadRetries;
        private bool _downgrade;
        const string WixHomePageUrl = "http://wixtoolset.org/";
        const string WixNewsUrl = "http://wixtoolset.org/news/";

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
            _root = root;
            _downloadRetries = new Dictionary<string, int>();

            _root.PropertyChanged += RootPropertyChanged;

            WixBootstrapperApplication.Model.Bootstrapper.DetectBegin += DetectBegin;
            WixBootstrapperApplication.Model.Bootstrapper.DetectRelatedBundle += DetectedRelatedBundle;
            WixBootstrapperApplication.Model.Bootstrapper.DetectPackageComplete += DetectedPackage;
            WixBootstrapperApplication.Model.Bootstrapper.DetectComplete += DetectComplete;
            WixBootstrapperApplication.Model.Bootstrapper.PlanPackageBegin += PlanPackageBegin;
            WixBootstrapperApplication.Model.Bootstrapper.PlanComplete += PlanComplete;
            WixBootstrapperApplication.Model.Bootstrapper.ApplyBegin += ApplyBegin;
            WixBootstrapperApplication.Model.Bootstrapper.Error += ExecuteError;
            WixBootstrapperApplication.Model.Bootstrapper.ResolveSource += ResolveSource;
            WixBootstrapperApplication.Model.Bootstrapper.ApplyComplete += ApplyComplete;
        }

        void RootPropertyChanged(object sender, PropertyChangedEventArgs e)
        {
            if ("State" == e.PropertyName)
            {
                base.OnPropertyChanged("Title");
                base.OnPropertyChanged("CompleteEnabled");
                base.OnPropertyChanged("ExitEnabled");
                base.OnPropertyChanged("RepairEnabled");
                base.OnPropertyChanged("InstallEnabled");
                base.OnPropertyChanged("UninstallEnabled");
            }
        }

        /// <summary>
        /// Gets the title for the application.
        /// </summary>
        public string Version 
        {
            get { return String.Concat("v", WixBootstrapperApplication.Model.Version.ToString()); }
        }

        public string Message
        {
            get
            {
                return _message;
            }

            set
            {
                if (_message != value)
                {
                    _message = value;
                    base.OnPropertyChanged("Message");
                }
            }
        }

        /// <summary>
        /// Gets and sets whether the view model considers this install to be a downgrade.
        /// </summary>
        public bool Downgrade
        {
            get
            {
                return _downgrade;
            }

            set
            {
                if (_downgrade != value)
                {
                    _downgrade = value;
                    base.OnPropertyChanged("Downgrade");
                }
            }
        }

        public ICommand LaunchHomePageCommand
        {
            get {
                return _launchHomePageCommand ??
                       (_launchHomePageCommand =
                        new RelayCommand(param => WixBootstrapperApplication.LaunchUrl(WixHomePageUrl), param => true));
            }
        }

        public ICommand LaunchNewsCommand
        {
            get {
                return _launchNewsCommand ??
                       (_launchNewsCommand =
                        new RelayCommand(param => WixBootstrapperApplication.LaunchUrl(WixNewsUrl), param => true));
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
            get {
                return _installCommand ??
                       (_installCommand =
                        new RelayCommand(param => Plan(LaunchAction.Install),
                                         param => _root.State == InstallationState.DetectedAbsent));
            }
        }

        public bool InstallEnabled
        {
            get { return InstallCommand.CanExecute(this); }
        }

        public ICommand RepairCommand
        {
            get {
                return _repairCommand ??
                       (_repairCommand =
                        new RelayCommand(param => Plan(LaunchAction.Repair),
                                         param => _root.State == InstallationState.DetectedPresent));
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
            get {
                return _uninstallCommand ??
                       (_uninstallCommand =
                        new RelayCommand(param => Plan(LaunchAction.Uninstall),
                                         param => _root.State == InstallationState.DetectedPresent));
            }
        }

        public bool UninstallEnabled
        {
            get { return UninstallCommand.CanExecute(this); }
        }

        public ICommand TryAgainCommand
        {
            get {
                return _tryAgainCommand ??
                       (_tryAgainCommand =
                        new RelayCommand(param => Plan(_plannedAction), param => _root.State == InstallationState.Failed));
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
            // TODO: verify that the engine is in a state that will allow it to do Detect().

            _root.Canceled = false;
            WixBootstrapperApplication.Model.Engine.Detect();
        }

        /// <summary>
        /// Launches the license in the default viewer.
        /// </summary>
        private static void LaunchLicense()
        {
            var folder = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
            WixBootstrapperApplication.LaunchUrl(Path.Combine(folder, "License.htm"));
        }

        /// <summary>
        /// Starts planning the appropriate action.
        /// </summary>
        /// <param name="action">Action to plan.</param>
        private void Plan(LaunchAction action)
        {
            _planAttempted = true;
            _plannedAction = action;
            _hwnd = (WixBootstrapperApplication.View == null) ? IntPtr.Zero : new WindowInteropHelper(WixBootstrapperApplication.View).Handle;

            _root.Canceled = false;
            WixBootstrapperApplication.Model.Engine.Plan(_plannedAction);
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
            if (e.PackageId.Equals("Wix", StringComparison.Ordinal))
            {

                _root.State = (e.State == PackageState.Present) ? InstallationState.DetectedPresent : InstallationState.DetectedAbsent;
            }
        }

        private void DetectComplete(object sender, DetectCompleteEventArgs e)
        {
            if (LaunchAction.Uninstall == WixBootstrapperApplication.Model.Command.Action)
            {
                WixBootstrapperApplication.Model.Engine.Log(LogLevel.Verbose, "Invoking automatic plan for uninstall");
                WixBootstrapperApplication.Dispatcher.Invoke((Action)(() => Plan(LaunchAction.Uninstall)));
            }
            else if (Hresult.Succeeded(e.Status))
            {
                if (Downgrade)
                {
                    // TODO: What behavior do we want for downgrade?
                    _root.State = InstallationState.DetectedNewer;
                }

                // If we're not waiting for the user to click install, dispatch plan with the default action.
                if (WixBootstrapperApplication.Model.Command.Display != Display.Full)
                {
                    WixBootstrapperApplication.Model.Engine.Log(LogLevel.Verbose, "Invoking automatic plan for non-interactive mode.");
                    WixBootstrapperApplication.Dispatcher.Invoke((Action)(() => Plan(WixBootstrapperApplication.Model.Command.Action)));
                }
            }
            else
            {
                _root.State = InstallationState.Failed;
            }
        }

        private static void PlanPackageBegin(object sender, PlanPackageBeginEventArgs e)
        {
            if (e.PackageId.Equals(WixBootstrapperApplication.Model.Engine.StringVariables["MbaNetfxPackageId"], StringComparison.Ordinal))
            {
                e.State = RequestState.None;
            }
        }

        private void PlanComplete(object sender, PlanCompleteEventArgs e)
        {
            if (Hresult.Succeeded(e.Status))
            {
                _root.PreApplyState = _root.State;
                _root.State = InstallationState.Applying;
                WixBootstrapperApplication.Model.Engine.Apply(_hwnd);
            }
            else
            {
                _root.State = InstallationState.Failed;
            }
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
                    {
                        _root.State = _root.PreApplyState;
                    }
                    else
                    {
                        Message = e.ErrorMessage;

                        WixBootstrapperApplication.View.Dispatcher.Invoke(
                                                                          (Action)
                                                                          (() =>
                                                                           MessageBox.Show(
                                                                                           WixBootstrapperApplication.
                                                                                                   View,
                                                                                           e.ErrorMessage,
                                                                                           "WiX Toolset",
                                                                                           MessageBoxButton.OK,
                                                                                           MessageBoxImage.Error)));
                    }
                }

                e.Result = _root.Canceled ? Result.Cancel : Result.Ok;
            }
        }

        private void ResolveSource(object sender, ResolveSourceEventArgs e)
        {
            int retries;

            _downloadRetries.TryGetValue(e.PackageOrContainerId, out retries);
            _downloadRetries[e.PackageOrContainerId] = retries + 1;

            e.Result = retries < 3 && !String.IsNullOrEmpty(e.DownloadSource) ? Result.Download : Result.Ok;
        }

        private void ApplyComplete(object sender, ApplyCompleteEventArgs e)
        {
            // If we're not in Full UI mode, we need to alert the dispatcher to stop and close the window for passive.
            if (Display.Full != WixBootstrapperApplication.Model.Command.Display)
            {
                // If its passive, send a message to the window to close.
                if (Display.Passive == WixBootstrapperApplication.Model.Command.Display)
                {
                    WixBootstrapperApplication.Model.Engine.Log(LogLevel.Verbose, "Automatically closing the window for non-interactive install");
                    WixBootstrapperApplication.Dispatcher.BeginInvoke((Action)(() => WixBootstrapperApplication.View.Close()));
                }
                else
                {
                    WixBootstrapperApplication.Dispatcher.InvokeShutdown();
                }
            }

            // Set the state to applied or failed unless the state has already been set back to the preapply state
            // which means we need to show the UI as it was before the apply started.
            if (_root.State != _root.PreApplyState)
            {
                _root.State = Hresult.Succeeded(e.Status) ? InstallationState.Applied : InstallationState.Failed;
            }
        }
    }
}
