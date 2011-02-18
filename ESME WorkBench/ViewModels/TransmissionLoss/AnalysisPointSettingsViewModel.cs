﻿using System;
using System.ComponentModel;
using System.Windows;
using System.Diagnostics;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    [ExportViewModel("AnalysisPointSettingsViewModel")]
    class AnalysisPointSettingsViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        readonly IMessageBoxService _messageBoxService;
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;

        public AnalysisPointSettingsViewModel(AnalysisPoint analysisPoint, IMessageBoxService messageBoxService)
        {
            RegisterMediator();
            _messageBoxService = messageBoxService;
            AnalysisPoint = analysisPoint;
        }

        #region public AnalysisPoint AnalysisPoint { get; set; }

        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                if (_analysisPoint == value) return;
                _analysisPoint = value;
                NotifyPropertyChanged(AnalysisPointChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.AnalysisPoint);
        AnalysisPoint _analysisPoint;

        #endregion

        #region public SoundSource SelectedMode { get; set; }

        public SoundSource SelectedMode
        {
            get { return _selectedMode; }
            set
            {
                if (_selectedMode == value) return;
                IsItemSelected = _selectedMode != null;
                _selectedMode = value;
                NotifyPropertyChanged(SelectedModeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedModeChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.SelectedMode);
        SoundSource _selectedMode;

        #endregion

        #region public float SelectedBearing { get; set; }

        public float SelectedBearing
        {
            get { return _selectedBearing; }
            set
            {
                if (_selectedBearing == value) return;
                _selectedBearing = value;
                NotifyPropertyChanged(SelectedBearingChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedBearingChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.SelectedBearing);
        float _selectedBearing;

        #endregion

        #region public bool IsItemSelected { get; set; }

        public bool IsItemSelected
        {
            get { return _isItemSelected; }
            set
            {
                if (_isItemSelected == value) return;
                _isItemSelected = value;
                NotifyPropertyChanged(IsItemSelectedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsItemSelectedChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.IsItemSelected);
        bool _isItemSelected;

        #endregion

        #region IViewStatusAwareInjectionAware
        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
        }
        #endregion

        #region Mediator Registration

        void RegisterMediator()
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nAnalysisPointSettingsViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
        }

        #endregion
    }
}
