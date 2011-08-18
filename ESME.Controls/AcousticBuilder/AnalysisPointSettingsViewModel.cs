using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Linq;
using Cinch;
using ESME.TransmissionLoss;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.AcousticBuilder
{
    [ExportViewModel("AnalysisPointSettingsViewModel")]
    public class AnalysisPointSettingsViewModel : ViewModelBase
    {
        public AnalysisPointSettingsViewModel(AnalysisPoint analysisPoint) : this()
        {
            AnalysisPoint = analysisPoint;
        }
        [ImportingConstructor]
        public AnalysisPointSettingsViewModel()
        {
            RegisterMediator();

            AvailableModes = new ObservableCollection<SoundSource>();
        }

        public static IMessageBoxService MessageBoxService { get; set; }

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
                if (_analysisPoint != null) _analysisPoint.Validate();
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

        #region public bool AnalysisPointIsChanged { get; set; }

        public bool AnalysisPointIsChanged
        {
            get { return _analysisPointIsChanged; }
            set
            {
                if (_analysisPointIsChanged == value) return;
                _analysisPointIsChanged = value;
                NotifyPropertyChanged(AnalysisPointIsChangedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs AnalysisPointIsChangedChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.AnalysisPointIsChanged);
        bool _analysisPointIsChanged;

        #endregion

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get { return _okCommand ?? (_okCommand = new SimpleCommand<object, object>(delegate
                                                                                       {
                                                                                           if (AnalysisPoint != null) AnalysisPoint.Validate();
                                                                                           CloseActivePopUpCommand.Execute(true);
                                                                                       })); }
        }

        SimpleCommand<object, object> _okCommand;

        #endregion

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancelCommand ?? (_cancelCommand = new SimpleCommand<object, object>(delegate
                                                                                               {
                                                                                                   AnalysisPointIsChanged = false;
                                                                                                   CloseActivePopUpCommand.Execute(false);
                                                                                               })); }
        }

        SimpleCommand<object, object> _cancelCommand;

        #endregion

        #region ApplyToAllModesCommand

        public SimpleCommand<object, object> ApplyToAllModesCommand
        {
            get
            {
                return _applyToAllModes ?? (_applyToAllModes = new SimpleCommand<object, object>(
                    delegate { return IsItemSelected && AvailableModes.Count > 0 && SelectedMode.BearingsStringIsValid; },
                    delegate
                    {
                        var result = MessageBoxService.ShowOkCancel("Are you sure you want to use this radial configuration\nfor all modes in this analysis point?", CustomDialogIcons.Question);
                        if (result == CustomDialogResults.OK)
                        {
                            var tmpRadials = new List<float>(SelectedMode.RadialBearings);
                            var radialsToSet = tmpRadials.Distinct().ToList();
                            radialsToSet.Sort();
                            AnalysisPointIsChanged = true;
                            foreach (var soundsource in AnalysisPoint.SoundSources)
                            {
                                soundsource.RadialBearings = new List<float>(radialsToSet);
                                soundsource.Validate();
                            }
                        }
                    }));
            }
        }

        SimpleCommand<object, object> _applyToAllModes;

        #endregion

        #region RadialsLostFocusCommand
        public SimpleCommand<object, object> RadialsLostFocusCommand
        {
            get
            {
                return _radialsLostFocus ??
                       (_radialsLostFocus =
                        new SimpleCommand<object, object>(delegate { foreach (var mode in AvailableModes) mode.BearingsString = null; }));
            }
        }

        SimpleCommand<object, object> _radialsLostFocus;

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