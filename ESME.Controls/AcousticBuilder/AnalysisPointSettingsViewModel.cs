using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Threading;
using Cinch;
using ESME.NEMO;
using ESME.TransmissionLoss;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.AcousticBuilder
{
    [ExportViewModel("AnalysisPointSettingsViewModel")]
    public class AnalysisPointSettingsViewModel : ViewModelBase, IViewStatusAwareInjectionAware, IDesignTimeAware
    {
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;
        int _selectedBearingIndex = -1;

        public AnalysisPointSettingsViewModel(AnalysisPoint analysisPoint)
        {
            RegisterMediator();

            AvailableModes = new ObservableCollection<SoundSource>();
            AvailableBearings = new ObservableCollection<float>();
            //TempAvailableModes = new ObservableCollection<SoundSource>();
            
            //AnalysisPoint = analysisPoint;
            TempAnalysisPoint = analysisPoint;
            SelectedBearing = null;
        }
        [ImportingConstructor]
        public AnalysisPointSettingsViewModel()
        {
            RegisterMediator();

            AvailableModes = new ObservableCollection<SoundSource>();
            AvailableBearings = new ObservableCollection<float>();
            //TempAvailableModes = new ObservableCollection<SoundSource>();

            //AnalysisPoint = analysisPoint;
            
            SelectedBearing = null;
        }

        public static IMessageBoxService MessageBoxService { get; set; }

        #region AnalysisPoint TempAnalysisPoint { get; set; }

        AnalysisPoint TempAnalysisPoint
        {
            get { return _tempAnalysisPoint; }
            set
            {
                if (_tempAnalysisPoint == value) return;
                _tempAnalysisPoint = value;
                AvailableModes.Clear();
                foreach (var soundSource in _tempAnalysisPoint.SoundSources) AvailableModes.Add(soundSource);//TempAvailableModes.Add(soundSource);
                NotifyPropertyChanged(TempAnalysisPointChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TempAnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.TempAnalysisPoint);
        AnalysisPoint _tempAnalysisPoint;

        #endregion

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
                RefreshAvailableBearings();
            }
        }

        static readonly PropertyChangedEventArgs SelectedModeChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.SelectedMode);
        SoundSource _selectedMode;

        void RefreshAvailableBearings()
        {
            AvailableBearings.Clear();
            if (_selectedMode != null)
            {
                foreach (var bearing in _selectedMode.RadialBearings) AvailableBearings.Add(bearing);
                _selectedMode.Validate();
            }
            SelectedBearing = null;
            DisplayedBearing = null;
        }

        #endregion

        #region public ObservableCollection<float> AvailableBearings { get; set; }

        public ObservableCollection<float> AvailableBearings
        {
            get { return _availableBearings; }
            set
            {
                if (_availableBearings == value) return;
                if (_availableBearings != null) _availableBearings.CollectionChanged -= AvailableBearingsCollectionChanged;
                _availableBearings = value;
                if (_availableBearings != null) _availableBearings.CollectionChanged += AvailableBearingsCollectionChanged;
                NotifyPropertyChanged(AvailableBearingsChangedEventArgs);
            }
        }

        void AvailableBearingsCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            NotifyPropertyChanged(AvailableBearingsChangedEventArgs);
        }
        static readonly PropertyChangedEventArgs AvailableBearingsChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.AvailableBearings);
        ObservableCollection<float> _availableBearings;

        #endregion

        #region public float? DisplayedBearing { get; set; }

        public float? DisplayedBearing
        {
            get { return _displayedBearing; }
            set
            {
                if (_displayedBearing == value) return;
                _displayedBearing = value;
                NotifyPropertyChanged(DisplayedBearingChangedEventArgs);

            }
        }

        static readonly PropertyChangedEventArgs DisplayedBearingChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointSettingsViewModel>(x => x.DisplayedBearing);
        float? _displayedBearing;

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
                if (_selectedBearing.HasValue) _selectedBearingIndex = _selectedMode.RadialBearings.IndexOf(_selectedBearing.Value);
                else _selectedBearingIndex = -1;
                DisplayedBearing = _selectedBearing;
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
                                                                                           AnalysisPoint = TempAnalysisPoint;
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

        #region AddRadialCommand

        public SimpleCommand<object, object> AddRadialCommand
        {
            get
            {
                return _addRadialCommand ??
                    (_addRadialCommand = new SimpleCommand<object, object>(
                        delegate { return (_selectedBearingIndex==-1 && DisplayedBearing.HasValue && !SelectedMode.RadialBearings.Contains(DisplayedBearing.Value)); },
                        delegate
                        {
                            //if the selected bearing is a float and does not already exist, add it. 
                            if (DisplayedBearing != null)
                            {
                                AnalysisPointIsChanged = true;
                                SelectedMode.RadialBearings.Add(DisplayedBearing.Value);
                                SelectedMode.RadialBearings.Sort();
                                RefreshAvailableBearings();
                            }
                            
                        }));
            }
        }

        SimpleCommand<object, object> _addRadialCommand;

        #endregion

        #region UpdateRadialCommand

        public SimpleCommand<object, object> UpdateRadialCommand
        {
            get
            {
                return _updateRadial ?? (_updateRadial = new SimpleCommand<object, object>(delegate
                                                                                           {
                                                                                               return (DisplayedBearing.HasValue && SelectedBearing.HasValue && DisplayedBearing.Value != SelectedBearing.Value && !SelectedMode.RadialBearings.Contains(DisplayedBearing.Value));
                                                                                           },
                      delegate
                      {
                          //if the selected bearing is a float, replace the old value with the new one.  

                          if (DisplayedBearing != null)
                          {
                              AnalysisPointIsChanged = true;
                              SelectedMode.RadialBearings[_selectedBearingIndex] = DisplayedBearing.Value;
                              SelectedMode.RadialBearings.Sort();
                              RefreshAvailableBearings();
                          }
                          
                      }));
            }
        }

        SimpleCommand<object, object> _updateRadial;

        #endregion

        #region CancelRadialEditCommand

        public SimpleCommand<object, object> CancelRadialEditCommand
        {
            get { return _cancelRadialEdit ?? (_cancelRadialEdit = new SimpleCommand<object, object>(delegate 
                { return DisplayedBearing.HasValue; }, 
                delegate { 
                    SelectedBearing = null;
                    DisplayedBearing = null;
                })); }
        }

        SimpleCommand<object, object> _cancelRadialEdit;

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
                            if (SelectedBearing.HasValue)
                            {
                                AnalysisPointIsChanged = true;
                                SelectedMode.RadialBearings.Remove(SelectedBearing.Value);
                                RefreshAvailableBearings();
                            }
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
                        var result = MessageBoxService.ShowOkCancel("Are you sure you want to use this radial configuration\nfor all modes in this analysis point?", CustomDialogIcons.Question);
                        if (result == CustomDialogResults.OK)
                        {
                            AnalysisPointIsChanged = true;
                            foreach (var soundsource in _tempAnalysisPoint.SoundSources)
                            {
                                soundsource.RadialBearings.Clear();
                                soundsource.RadialBearings.AddRange(AvailableBearings);
                                soundsource.RadialBearings.Sort();
                                soundsource.Validate();
                            }

                        }
                    }));
            }
        }

        SimpleCommand<object, object> _applyToAllModes;

        #endregion

        #region SelectedRadialTextChangedCommand

        public SimpleCommand<object, object> SelectedRadialTextChangedCommand
        {
            get
            {
                return _selectedRadialTextChanged ?? (_selectedRadialTextChanged = new SimpleCommand<object, object>(delegate(object cinchArgs)
                                                                                                                       {
                                                                                                                           var sender = (TextBox)((EventToCommandArgs)cinchArgs).Sender;
                                                                                                                           //var args = (TextChangedEventArgs)((EventToCommandArgs)cinchArgs).EventArgs;
                                                                                                                           if (sender != null && !string.IsNullOrEmpty(sender.Text)) DisplayedBearing = float.Parse(sender.Text);
                                                                                                                       }));
            }
        }

        SimpleCommand<object, object> _selectedRadialTextChanged;

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

        public void DesignTimeInitialization()
        {
            AvailableModes.Add(new SoundSource(new EarthCoordinate(0, 0), new NemoMode
                                                                          {
                                                                              HighFrequency = 1000,
                                                                              LowFrequency = 100,
                                                                              DepressionElevationAngle = 0,
                                                                              SourceDepth = 10,
                                                                              Name = "Design Test Mode",
                                                                              VerticalBeamWidth = 90,
                                                                              Radius = 100000,
                                                                          }, 4));
            AvailableBearings.Add(0);
            AvailableBearings.Add(90);
            AvailableBearings.Add(180);
            AvailableBearings.Add(360);

        }
    }
}