using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Threading.Tasks;
using Cinch;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.REFMS;
using HRC.Collections;
using HRC.Navigation;
using HRC.Validation;
using MEFedMVVM.ViewModelLocator;

namespace ESME.Views.AcousticBuilder
{
    [ExportViewModel("ExplosivePointPropertiesViewModel")]
    public sealed class ExplosivePointPropertiesViewModel : ValidatingViewModel
    {
        public ExplosivePointPropertiesViewModel(ExplosivePoint explosivePoint)
        {
            AvailableModes = new ObservableCollection<SoundSource>();
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
            explosivePoint.OldLocation = new Geo(explosivePoint);
            ExplosivePoint = explosivePoint;
            EnvironmentData = ExplosivePoint.EnvironmentData;
            Latitude = ExplosivePoint.Latitude;
            Longitude = ExplosivePoint.Longitude;
            UpdateEnvironmentData();
            DepthLimit = explosivePoint.WaterDepth;
            if (DepthLimit > 2000) DepthLimitEnabled = true;
            SourceCount = AvailableModes.Count;
            BottomReflectionsEnabled = explosivePoint.BottomReflectionsEnabled;
            BottomExponentialEnabled = DepthLimit < 2000;
            IsEnabled = true;
            Delta = explosivePoint.Delta;
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
                UpdateEnvironmentData();
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
                UpdateEnvironmentData();
            }
        }

        static readonly PropertyChangedEventArgs LongitudeChangedEventArgs = ObservableHelper.CreateArgs<AnalysisPointPropertiesViewModel>(x => x.Longitude);
        double _longitude;

        #endregion

        #region public bool DepthLimitEnabled { get; set; }

        public bool DepthLimitEnabled
        {
            get { return _depthLimitEnabled; }
            set
            {
                if (_depthLimitEnabled == value) return;
                _depthLimitEnabled = value;
                NotifyPropertyChanged(DepthLimitEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthLimitEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.DepthLimitEnabled);
        bool _depthLimitEnabled;

        #endregion

        #region public double DepthLimit { get; set; }

        public double DepthLimit
        {
            get { return _depthLimit; }
            set
            {
                if (_depthLimit == value) return;
                _depthLimit = value;
                NotifyPropertyChanged(DepthLimitChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs DepthLimitChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.DepthLimit);
        double _depthLimit;

        #endregion

        #region public int SourceCount { get; set; }

        public int SourceCount
        {
            get { return _sourceCount; }
            set
            {
                if (_sourceCount == value) return;
                _sourceCount = value;
                NotifyPropertyChanged(SourceCountChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SourceCountChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.SourceCount);
        int _sourceCount;

        #endregion

        #region public bool BottomReflectionsEnabled { get; set; }

        public bool BottomReflectionsEnabled
        {
            get { return _bottomReflectionsEnabled; }
            set
            {
                if (_bottomReflectionsEnabled == value) return;
                _bottomReflectionsEnabled = value;
                NotifyPropertyChanged(BottomReflectionsEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BottomReflectionsEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.BottomReflectionsEnabled);
        bool _bottomReflectionsEnabled;

        #endregion

        #region public bool BottomExponentialEnabled { get; set; }

        public bool BottomExponentialEnabled
        {
            get { return _bottomExponentialEnabled; }
            set
            {
                if (_bottomExponentialEnabled == value) return;
                _bottomExponentialEnabled = value;
                NotifyPropertyChanged(BottomExponentialEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BottomExponentialEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.BottomExponentialEnabled);
        bool _bottomExponentialEnabled;

        #endregion

        #region public bool IsEnabled { get; set; }

        public bool IsEnabled
        {
            get { return _isEnabled; }
            set
            {
                if (_isEnabled == value) return;
                _isEnabled = value;
                NotifyPropertyChanged(IsEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.IsEnabled);
        bool _isEnabled;

        #endregion

        #region public double Delta { get; set; }

        public double Delta
        {
            get { return _delta; }
            set
            {
                if (_delta == value) return;
                _delta = value;
                NotifyPropertyChanged(DeltaChangedEventArgs);
                UpdateEnvironmentData();
            }
        }

        static readonly PropertyChangedEventArgs DeltaChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.Delta);
        double _delta;

        #endregion

        #region public SVPFile SVPFile { get; set; }

        public SVPFile SVPFile
        {
            get { return _svpFile; }
            set
            {
                if (_svpFile == value) return;
                _svpFile = value;
                NotifyPropertyChanged(SVPFileChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SVPFileChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.SVPFile);
        SVPFile _svpFile;

        #endregion

        #region public double ProfileDepth { get; set; }

        public double ProfileDepth
        {
            get { return _profileDepth; }
            set
            {
                if (_profileDepth == value) return;
                _profileDepth = value;
                NotifyPropertyChanged(ProfileDepthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ProfileDepthChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.ProfileDepth);
        double _profileDepth;

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
                                                 ExplosivePoint.Delta = Delta;
                                                 if (DepthLimitEnabled) ExplosivePoint.WaterDepth = DepthLimit;
                                                 ExplosivePoint.BottomReflectionsEnabled = BottomReflectionsEnabled;
                                                 ExplosivePoint.BottomExponentialEnabled = BottomExponentialEnabled;

                                                 if (ExplosivePoint != null) ExplosivePoint.Validate();
                                                 
                                                 ExplosivePoint.UpdateEnvironmentData();
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
                                                                                                   CloseActivePopUpCommand.Execute(false);
                                                                                               })); }
        }

        SimpleCommand<object, object> _cancelCommand;

        #endregion

        #region public ObservableConcurrentDictionary<EnvironmentDataType, Task> EnvironmentData { get; set; }

        public ObservableConcurrentDictionary<EnvironmentDataType, Task> EnvironmentData
        {
            get { return _environmentData; }
            set
            {
                if (_environmentData == value) return;
                _environmentData = value;
                NotifyPropertyChanged(EnvironmentDataChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs EnvironmentDataChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.EnvironmentData);
        ObservableConcurrentDictionary<EnvironmentDataType, Task> _environmentData;

        #endregion

        #region public Geo SVPLocation { get; set; }

        public Geo SVPLocation
        {
            get { return _svpLocation; }
            set
            {
                if (_svpLocation == value) return;
                _svpLocation = value;
                NotifyPropertyChanged(SVPLocationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SVPLocationChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.SVPLocation);
        Geo _svpLocation;

        #endregion

        #region public double SVPWaterDepth { get; set; }

        public double SVPWaterDepth
        {
            get { return _svpWaterDepth; }
            set
            {
                if (_svpWaterDepth == value) return;
                _svpWaterDepth = value;
                NotifyPropertyChanged(SVPWaterDepthChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SVPWaterDepthChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.SVPWaterDepth);
        double _svpWaterDepth;

        #endregion

        public void UpdateEnvironmentData()
        {
            var curLocation = new Geo(Latitude, Longitude);
            var tempData = ((Task<SoundSpeed>)EnvironmentData[EnvironmentDataType.Temperature]).Result[ExplosivePoint.TimePeriod].EnvironmentData[curLocation];
            var salData = ((Task<SoundSpeed>)EnvironmentData[EnvironmentDataType.Salinity]).Result[ExplosivePoint.TimePeriod].EnvironmentData[curLocation];
            var sspData = ((Task<SoundSpeed>)EnvironmentData[EnvironmentDataType.SoundSpeed]).Result[ExplosivePoint.TimePeriod].EnvironmentData[curLocation];
            SVPLocation = new Geo(sspData);
            NotifyPropertyChanged(SVPFilenameChangedEventArgs);
            var bottomLossData = ((Task<BottomLoss>)EnvironmentData[EnvironmentDataType.BottomLoss]).Result.Samples[curLocation].Data;
            DepthLimit = Math.Abs(((Task<Bathymetry>)EnvironmentData[EnvironmentDataType.Bathymetry]).Result.Samples[curLocation].Data);
            SVPWaterDepth = Math.Abs(((Task<Bathymetry>)EnvironmentData[EnvironmentDataType.Bathymetry]).Result.Samples[SVPLocation].Data);

            var temperatureData = new double[tempData.Data.Count];
            var depthData = new double[tempData.Data.Count];
            var salinityData = new double[tempData.Data.Count];
            var soundSpeedData = new double[tempData.Data.Count];
            for (var i = 0; i < tempData.Data.Count; i++)
            {
                temperatureData[i] = tempData.Data[i].Value;
                depthData[i] = tempData.Data[i].Depth;
                salinityData[i] = salData.Data[i].Value;
                soundSpeedData[i] = sspData.Data[i].Value;
            }
            ProfileDepth = tempData.Data.Last().Depth;
            SVPFile = SVPFile.Create(SVPLocation, depthData, temperatureData, salinityData, soundSpeedData, bottomLossData, Delta);
        }

        public string SVPFilename
        {
            get
            {
                if (SVPLocation == null) return null;
                var northSouth = SVPLocation.Latitude >= 0 ? "N" : "S";
                var eastWest = SVPLocation.Longitude >= 0 ? "E" : "W";
                return string.Format("LOC_{0}{1}_{2}{3}_{4:0}-{5}", DegreesMinutes(SVPLocation.Latitude), northSouth, DegreesMinutes(SVPLocation.Longitude), eastWest, SVPWaterDepth, ExplosivePoint.TimePeriod);
            }
        }
        static readonly PropertyChangedEventArgs SVPFilenameChangedEventArgs = ObservableHelper.CreateArgs<ExplosivePointPropertiesViewModel>(x => x.SVPFilename);

        static string DegreesMinutes(double itude)
        {
            var degrees = (int)(Math.Abs(itude));
            var fraction = ((int)((Math.Abs(itude) - degrees) * 100)) / 100.0;
            var minutes = (int)(fraction * 60.0);
            return string.Format("{0}{1:00}", degrees, minutes);
        }


    }
}