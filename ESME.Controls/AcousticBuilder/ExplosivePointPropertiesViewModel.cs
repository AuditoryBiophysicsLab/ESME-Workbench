using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Xml.Serialization;
using Cinch;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.REFMS;
using HRC.Validation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.AcousticBuilder
{
    [ExportViewModel("ExplosivePointPropertiesViewModel")]
    public sealed class ExplosivePointPropertiesViewModel : ValidatingViewModel
    {
        public ExplosivePointPropertiesViewModel(ExplosivePoint explosivePoint)
        {
            RegisterMediator();
            AvailableModes = new ObservableCollection<SoundSource>();
            ExplosivePoint = explosivePoint;
            IsItemSelected = false;
            Latitude = ExplosivePoint.Latitude;
            Longitude = ExplosivePoint.Longitude;
            
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "Latitude",
                    Description = "Latitude is out of range",
                    RuleDelegate = (o, r) =>RangeCheck(((ExplosivePointPropertiesViewModel)o).Latitude, -90, 90),
                },
                new ValidationRule
                {
                    PropertyName = "Longitude",
                    Description = "Longitude is out of range",
                    RuleDelegate = (o, r) =>RangeCheck(((ExplosivePointPropertiesViewModel)o).Longitude, -180, 180),
                },
            });
        }

        public static IMessageBoxService MessageBoxService { get; set; }

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

        #region public ExplosivePoint ExplosivePoint { get; set; }

        public ExplosivePoint ExplosivePoint
        {
            get { return _explosivePoint; }
            set
            {
                if (_explosivePoint == value) return;
                _explosivePoint = value;
                AvailableModes.Clear();
                foreach (var soundSource in _explosivePoint.SoundSources) AvailableModes.Add(soundSource);
                NotifyPropertyChanged(AnalysisPointChangedEventArgs);
                if (_explosivePoint != null) _explosivePoint.Validate();
            }
        }

        static readonly PropertyChangedEventArgs AnalysisPointChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointPropertiesViewModel>(x => x.AnalysisPoint);
        ExplosivePoint _explosivePoint;

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
                                                 ExplosivePoint.Latitude = Latitude;
                                                 ExplosivePoint.Longitude = Longitude;
                                                 if (ExplosivePoint != null) ExplosivePoint.Validate();
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

        #region RegenerateCommand
        public SimpleCommand<object, object> RegenerateCommand
        {
            get
            {
                return _regenerate ??
                       (_regenerate =
                        new SimpleCommand<object, object>(delegate { return IsRegenerateCommandEnabled; },
                                                          delegate { RegenerateHandler(); }));
            }
        }

        SimpleCommand<object, object> _regenerate;

        bool IsRegenerateCommandEnabled
        {
            get { return true; }
        }

        void RegenerateHandler() { }
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