//-------------------------------------------------------------------------------------------------
// <copyright file="RootViewModel.cs" company="Microsoft">
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
// The model of the view for the WixBA.
// </summary>
//-------------------------------------------------------------------------------------------------

using System.Windows;
using System.Windows.Input;
using Microsoft.Tools.WindowsInstallerXml.Bootstrapper;

namespace WixBootstrapper
{
    /// <summary>
    /// The errors returned from the engine
    /// </summary>
    public enum Error
    {
        UserCancelled = 1223,
    }

    /// <summary>
    /// The model of the root view in WixBA.
    /// </summary>
    public class RootViewModel : PropertyNotifyBase
    {
        ICommand _cancelCommand;
        ICommand _closeCommand;
        ICommand _refreshCommand;

        bool _canceled;
        InstallationState _state;

        /// <summary>
        /// Creates a new model of the root view.
        /// </summary>
        public RootViewModel()
        {
            ESMEBootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, "Entering RootViewModel constructor");
            InstallationViewModel = new InstallationViewModel(this);
            ProgressViewModel = new ProgressViewModel(this);
            UpdateViewModel = new UpdateViewModel(this);
            ESMEBootstrapper.Model.Bootstrapper.Engine.Log(LogLevel.Verbose, "Exiting RootViewModel constructor");
        }

        public InstallationViewModel InstallationViewModel { get; private set; }
        public ProgressViewModel ProgressViewModel { get; private set; }
        public UpdateViewModel UpdateViewModel { get; private set; }

        public ICommand CloseCommand { get { return _closeCommand ?? (_closeCommand = new RelayCommand(param => ESMEBootstrapper.View.Close())); } }

        public ICommand RefreshCommand { get { return _refreshCommand ?? (_refreshCommand = new RelayCommand(param => Refresh(), param => false)); } }

        public ICommand CancelCommand
        {
            get
            {
                return _cancelCommand ?? (_cancelCommand = new RelayCommand(param =>
                {
                    lock (this) 
                    { 
                        Canceled = (MessageBoxResult.Yes == MessageBox.Show(ESMEBootstrapper.View, "Are you sure you want to cancel?", "WiX Toolset", MessageBoxButton.YesNo, MessageBoxImage.Error));
                    }
                },
                param => State == InstallationState.Applying));
            }
        }

        public bool CancelEnabled
        {
            get { return CancelCommand.CanExecute(this); }
        }

        public bool Canceled
        {
            get { return _canceled; }

            set
            {
                if (_canceled == value) return;
                _canceled = value;
                base.OnPropertyChanged("Canceled");
            }
        }

        /// <summary>
        /// Gets and sets the state of the view's model.
        /// </summary>
        public InstallationState State
        {
            get { return _state; }

            set
            {
                if (_state == value) return;
                _state = value;

                // Notify all the properties derived from the state that the state changed.
                base.OnPropertyChanged("State");
                base.OnPropertyChanged("CancelEnabled");
            }
        }

        /// <summary>
        /// Gets and sets the state of the view's model before apply begins in order to return to that state if cancel or rollback occurs.
        /// </summary>
        public InstallationState PreApplyState { get; set; }

        /// <summary>
        /// Instructs the various child models to refresh. Called directly via
        /// the UX *once* to initialize all the models. After that, only called
        /// when the RefreshCommand is executed.
        /// </summary>
        public void Refresh()
        {
            InstallationViewModel.Refresh();
            UpdateViewModel.Refresh();
        }

        public string ProductShortName { get; set; }
        public string ProductLongName { get; set; }

        #region public Visibility MainPanelVisibility { get; set; }

        public Visibility MainPanelVisibility
        {
            get { return _mainPanelVisibility; }
            set
            {
                if (_mainPanelVisibility == value) return;
                _mainPanelVisibility = value;
                OnPropertyChanged("MainPanelVisibility");
            }
        }

        Visibility _mainPanelVisibility = Visibility.Visible;

        #endregion

        #region public Visibility OptionsPanelVisibility { get; set; }

        public Visibility OptionsPanelVisibility
        {
            get { return _optionsPanelVisibility; }
            set
            {
                if (_optionsPanelVisibility == value) return;
                _optionsPanelVisibility = value;
                OnPropertyChanged("OptionsPanelVisibility");
            }
        }

        Visibility _optionsPanelVisibility = Visibility.Collapsed;

        #endregion
    }
}
