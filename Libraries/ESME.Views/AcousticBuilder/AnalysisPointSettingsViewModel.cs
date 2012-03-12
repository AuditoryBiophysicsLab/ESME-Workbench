using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.Linq;
using System.Xml.Serialization;
using Cinch;
using ESME.TransmissionLoss;
using HRC.Navigation;
using HRC.Validation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.AcousticBuilder
{
    [ExportViewModel("AnalysisPointPropertiesViewModel")]
    public sealed class AnalysisPointPropertiesViewModel : ValidatingViewModel
    {
        public AnalysisPointPropertiesViewModel(AnalysisPoint analysisPoint)
        {
            RegisterMediator();
            AvailableModes = new ObservableCollection<SoundSource>();
            AnalysisPoint = analysisPoint;
            IsItemSelected = false;
            Latitude = AnalysisPoint.Latitude;
            Longitude = AnalysisPoint.Longitude;
            AnalysisPoint.OldLocation = new Geo(AnalysisPoint);
            
            ValidationRules.AddRange(new List<ValidationRule>
            {
                    new ValidationRule
                    {
                            PropertyName = "Latitude",
                            Description = "Latitude is out of range",
                            RuleDelegate = (o, r) =>RangeCheck(((AnalysisPointPropertiesViewModel)o).Latitude, -90, 90),
                    },
                    new ValidationRule
                    {
                            PropertyName = "Longitude",
                            Description = "Longitude is out of range",
                            RuleDelegate = (o, r) =>RangeCheck(((AnalysisPointPropertiesViewModel)o).Longitude, -180, 180),
                    },
            });
        }

        [Import] static IMessageBoxService _messageBoxService;

        #region public double Latitude { get; set; }

        public double Latitude
        {
            get { return _latitude; }
            set
            {
                _latitude = value;
                NotifyPropertyChanged(LatitudeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LatitudeChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointPropertiesViewModel>(x => x.Latitude);
        double _latitude;

        #endregion

        #region public double Longitude { get; set; }

        public double Longitude
        {
            get { return _longitude; }
            set
            {
                _longitude = value;
                NotifyPropertyChanged(LongitudeChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs LongitudeChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointPropertiesViewModel>(x => x.Longitude);
        double _longitude;

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
            }
        }

        static readonly PropertyChangedEventArgs AnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointPropertiesViewModel>(x => x.AnalysisPoint);
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
        static readonly PropertyChangedEventArgs AvailableModesChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointPropertiesViewModel>(x => x.AvailableModes);
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

        static readonly PropertyChangedEventArgs SelectedModeChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointPropertiesViewModel>(x => x.SelectedMode);
        SoundSource _selectedMode;

        #endregion

        #region public bool IsItemSelected { get; set; }
        [XmlIgnore]
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

        static readonly PropertyChangedEventArgs IsItemSelectedChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointPropertiesViewModel>(x => x.IsItemSelected);
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

        static readonly PropertyChangedEventArgs AnalysisPointIsChangedChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointPropertiesViewModel>(x => x.AnalysisPointIsChanged);
        bool _analysisPointIsChanged;

        #endregion

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get
            {
                return _okCommand ??
                       (_okCommand = new SimpleCommand<object, object>(
                                             delegate { return IsValid; },
                                             delegate
                                             {
                                                 AnalysisPoint.Latitude = Latitude;
                                                 AnalysisPoint.Longitude = Longitude;
                                                 CloseActivePopUpCommand.Execute(true);
                                             }));
            }
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
                        var result = _messageBoxService.ShowOkCancel("Are you sure you want to use this radial configuration\nfor all modes in this analysis point?", CustomDialogIcons.Question);
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
                        new SimpleCommand<object, object>(delegate
                        {
                            foreach (var mode in AvailableModes) mode.BearingsString = null;
                        }));
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