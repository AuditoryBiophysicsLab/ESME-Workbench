﻿using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss;
using MEFedMVVM.ViewModelLocator;

namespace ESMEWorkBench.ViewModels.TransmissionLoss
{
    [ExportViewModel("AnalysisPointSettingsViewModel")]
    internal class AnalysisPointSettingsViewModel : ViewModelBase, IViewStatusAwareInjectionAware
    {
        readonly IMessageBoxService _messageBoxService;
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;

        public AnalysisPointSettingsViewModel(AnalysisPoint analysisPoint, IMessageBoxService messageBoxService)
        {
            RegisterMediator();
            _messageBoxService = messageBoxService;
            AvailableModes = new ObservableCollection<SoundSource>();
            AnalysisPoint = analysisPoint;
            SelectedBearing = null;
        }

        #region public AnalysisPoint AnalysisPoint { get; set; }

        public AnalysisPoint AnalysisPoint
        {
            get { return _analysisPoint; }
            set
            {
                if (_analysisPoint == value) return;
                _analysisPoint = value;
                AvailableModes.Clear();
                foreach (var soundSource in _analysisPoint.SoundSources) AvailableModes.Add(soundSource);
                NotifyPropertyChanged(AnalysisPointChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.AnalysisPoint);
        AnalysisPoint _analysisPoint;

        #endregion

        #region public ObservableCollection<SoundSource> AvailableModes { get; set; }

        public ObservableCollection<SoundSource> AvailableModes
        {
            get { return _availableModes; }
            set
            {
                if (_availableModes == value) return;
                if (_availableModes != null) _availableModes.CollectionChanged -= AvailableModesCollectionChanged;
                _availableModes = value;
                if (_availableModes != null) _availableModes.CollectionChanged += AvailableModesCollectionChanged;
                NotifyPropertyChanged(AvailableModesChangedEventArgs);
            }
        }

        void AvailableModesCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(AvailableModesChangedEventArgs); }
        static readonly PropertyChangedEventArgs AvailableModesChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.AvailableModes);
        ObservableCollection<SoundSource> _availableModes;

        #endregion

        #region public SoundSource SelectedMode { get; set; }

        public SoundSource SelectedMode
        {
            get { return _selectedMode; }
            set
            {
                if (_selectedMode == value) return;
                _selectedMode = value;
                NotifyPropertyChanged(SelectedModeChangedEventArgs);
                IsItemSelected = _selectedMode != null;
            }
        }

        static readonly PropertyChangedEventArgs SelectedModeChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.SelectedMode);
        SoundSource _selectedMode;

        #endregion

        #region public float? SelectedBearing { get; set; }

        public float? SelectedBearing
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
        float? _selectedBearing;

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

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get { return _okCommand ?? (_okCommand = new SimpleCommand<object, object>(delegate { CloseActivePopUpCommand.Execute(true); })); }
        }

        SimpleCommand<object, object> _okCommand;

        #endregion

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancelCommand ?? (_cancelCommand = new SimpleCommand<object, object>(delegate { CloseActivePopUpCommand.Execute(false); })); }
        }

        SimpleCommand<object, object> _cancelCommand;

        #endregion

        #region AddRadialCommand

        public SimpleCommand<object, object> AddRadialCommand
        {
            get 
            { 
                return _addRadialCommand ?? 
                    (_addRadialCommand = new SimpleCommand<object, object>(
                        delegate { return IsItemSelected; }, 
                        delegate { ; })); 
            }
        }

        SimpleCommand<object, object> _addRadialCommand;

        #endregion

        #region RemoveRadialCommand

        public SimpleCommand<object, object> RemoveRadialCommand
        {
            get 
            { 
                return _removeRadialCommand ?? 
                    (_removeRadialCommand = new SimpleCommand<object, object>(
                        delegate { return IsItemSelected & SelectedBearing.HasValue; },
                        delegate
                        {
                            if (SelectedBearing.HasValue) SelectedMode.RadialBearings.Remove(SelectedBearing.Value);
                        }));
            }
        }

        SimpleCommand<object, object> _removeRadialCommand;

        #endregion

        #region ApplyToAllModesCommand

        public SimpleCommand<object, object> ApplyToAllModesCommand
        {
            get
            {
                return _applyToAllModes ?? (_applyToAllModes = new SimpleCommand<object, object>(
                    delegate
                    {
                        var result = _messageBoxService.ShowOkCancel("Are you sure you want to use this radial configuration\nfor all modes in this analysis point?", CustomDialogIcons.Question);
                        if (result == CustomDialogResults.Yes)
                        {
                            // todo: Apply the changes to all modes
                        }
                    }));
            }
        }

        SimpleCommand<object, object> _applyToAllModes;

        #endregion

        #region IViewStatusAwareInjectionAware

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window) _viewAwareStatus.View).Dispatcher;
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