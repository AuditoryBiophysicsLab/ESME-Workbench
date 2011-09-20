using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Threading.Tasks;
using Cinch;
using ESME.Environment;
using ESME.Environment.Descriptors;
using ESME.Environment.NAVO;
using HRC.Collections;

namespace ESME.Views.EnvironmentBuilder
{
    public class EnvironmentSelectionViewModel : ViewModelBase
    {
        readonly RangeComplexes _rangeComplexes;
        public EnvironmentSelectionViewModel(RangeComplexes rangeComplexes) 
        {
            _rangeComplexes = rangeComplexes;
            RangeComplexes = _rangeComplexes.RangeComplexCollection;
            _availableTimePeriods = new ObservableConcurrentDictionary<string, NAVOTimePeriod>
            {
                {null, NAVOTimePeriod.Invalid},
            };
            foreach (var month in NAVOConfiguration.AllMonths) _availableTimePeriods.Add(month.ToString(), month);
            foreach (var season in NAVOConfiguration.AllSeasons) _availableTimePeriods.Add(season.ToString(), season);
        }

        public ObservableConcurrentDictionary<string, NewRangeComplex> RangeComplexes { get; private set; }

        // A dictionary of all legal time periods, plus a null value
        #region public ObservableConcurrentDictionary<string, NAVOTimePeriod> AvailableTimePeriods { get; set; }

        public ObservableConcurrentDictionary<string, NAVOTimePeriod> AvailableTimePeriods
        {
            get { return _availableTimePeriods; }
        }

        readonly ObservableConcurrentDictionary<string, NAVOTimePeriod> _availableTimePeriods;

        #endregion

        // The name (key) of the selected range complex
        #region public string SelectedRangeComplexName { get; set; }

        public string SelectedRangeComplexName
        {
            get { return _selectedRangeComplexName; }
            set
            {
                if (_selectedRangeComplexName == value) return;
                _selectedRangeComplexName = value;
                NotifyPropertyChanged(SelectedRangeComplexNameChangedEventArgs);
                SelectedRangeComplex = _selectedRangeComplexName == null ? null : _rangeComplexes.RangeComplexCollection[_selectedRangeComplexName];
            }
        }

        static readonly PropertyChangedEventArgs SelectedRangeComplexNameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSelectionViewModel>(x => x.SelectedRangeComplexName);
        string _selectedRangeComplexName;

        #endregion

        // The name (key) of the selected area of the selected range complex
        #region public string SelectedAreaName { get; set; }

        public string SelectedAreaName
        {
            get { return _selectedAreaName; }
            set
            {
                if (_selectedAreaName == value) return;
                _selectedAreaName = value;
                NotifyPropertyChanged(SelectedAreaNameChangedEventArgs);
                SelectedArea = _selectedAreaName == null ? null : SelectedRangeComplex.AreaCollection[_selectedAreaName];
            }
        }

        static readonly PropertyChangedEventArgs SelectedAreaNameChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSelectionViewModel>(x => x.SelectedAreaName);
        string _selectedAreaName;

        #endregion

        // The name (key) of the selected bathymetry resolution within the selected area of the selected range complex
        #region public string SelectedResolution { get; set; }

        public string SelectedResolution
        {
            get { return _selectedResolution; }
            set
            {
                if (_selectedResolution == value) return;
                _selectedResolution = value;
                NotifyPropertyChanged(SelectedResolutionChangedEventArgs);
                _bathymetryFile = _selectedResolution == null ? null : (BathymetryFile)SelectedArea.BathymetryFiles[_selectedResolution];
            }
        }

        static readonly PropertyChangedEventArgs SelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSelectionViewModel>(x => x.SelectedResolution);
        string _selectedResolution;

        #endregion

        // The name (key) of the selected time period
        #region public string SelectedTimePeriod { get; set; }

        public string SelectedTimePeriod
        {
            get { return _selectedTimePeriod; }
            set
            {
                if (_selectedTimePeriod == value) return;
                _selectedTimePeriod = value;
                NotifyPropertyChanged(SelectedTimePeriodChangedEventArgs);
                SelectedMonths = _selectedTimePeriod == null ? null : Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(AvailableTimePeriods[SelectedTimePeriod]).ToList();
            }
        }

        static readonly PropertyChangedEventArgs SelectedTimePeriodChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSelectionViewModel>(x => x.SelectedTimePeriod);
        string _selectedTimePeriod;

        #endregion

        // The currently selected range complex
        #region public NewRangeComplex SelectedRangeComplex { get; private set; }

        public NewRangeComplex SelectedRangeComplex
        {
            get { return _selectedRangeComplex; }
            private set
            {
                if (_selectedRangeComplex == value) return;
                _selectedRangeComplex = value;
                NotifyPropertyChanged(SelectedRangeComplexChangedEventArgs);
                SelectedAreaName = null;
                SelectedTimePeriod = null;
            }
        }

        static readonly PropertyChangedEventArgs SelectedRangeComplexChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSelectionViewModel>(x => x.SelectedRangeComplex);
        NewRangeComplex _selectedRangeComplex;

        #endregion

        // The currently selected area within the selected range complex
        #region public RangeComplexArea SelectedArea { get; private set; }

        public RangeComplexArea SelectedArea
        {
            get { return _selectedArea; }
            private set
            {
                if (_selectedArea == value) return;
                _selectedArea = value;
                NotifyPropertyChanged(SelectedAreaChangedEventArgs);
                if (_selectedArea == null) SelectedResolution = null;
            }
        }

        static readonly PropertyChangedEventArgs SelectedAreaChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSelectionViewModel>(x => x.SelectedArea);
        RangeComplexArea _selectedArea;

        #endregion

        #region private List<NAVOTimePeriod> SelectedMonths { get; set; }

        List<NAVOTimePeriod> SelectedMonths
        {
            get { return _selectedMonths; }
            set
            {
                if (_selectedMonths == value) return;
                _selectedMonths = value;
                NotifyPropertyChanged(SelectedMonthsChangedEventArgs);
                if (_selectedMonths == null)
                {
                    _temperatureFiles.Clear();
                    _salinityFiles.Clear();
                    return;
                }
                _temperatureFiles = (from temperatureFile in SelectedRangeComplex.TemperatureFile.Months.Values
                                     where _selectedMonths.Contains(temperatureFile.TimePeriod)
                                     select temperatureFile).ToDictionary(item => item.TimePeriod);
                _salinityFiles = (from salinityFile in SelectedRangeComplex.SalinityFile.Months.Values
                                  where _selectedMonths.Contains(salinityFile.TimePeriod)
                                  select salinityFile).ToDictionary(item => item.TimePeriod);
            }
        }

        static readonly PropertyChangedEventArgs SelectedMonthsChangedEventArgs = ObservableHelper.CreateArgs<EnvironmentSelectionViewModel>(x => x.SelectedMonths);
        List<NAVOTimePeriod> _selectedMonths;

        #endregion

        BathymetryFile _bathymetryFile;
        Dictionary<NAVOTimePeriod, EnvironmentFile<SoundSpeed>> _temperatureFiles;
        Dictionary<NAVOTimePeriod, EnvironmentFile<SoundSpeed>> _salinityFiles;

        public Bathymetry Bathymetry { get { return _bathymetryFile.DataTask.Result; } }
        public Wind Wind { get { return SelectedRangeComplex.WindFile.DataTask.Result; } }
        public BottomLoss BottomLoss { get { return SelectedRangeComplex.BottomLossFile.DataTask.Result; } }
        public Sediment Sediment { get { return SelectedRangeComplex.SedimentFile.DataTask.Result; } }
        public SoundSpeedField SoundSpeedField { get { return SoundSpeedFieldTask.Result; } }

        public Task<Bathymetry> BathymetryTask { get { return _bathymetryFile.DataTask; } }
        public Task<Wind> WindTask { get { return SelectedRangeComplex.WindFile.DataTask; } }
        public Task<BottomLoss> BottomLossTask { get { return SelectedRangeComplex.BottomLossFile.DataTask; } }
        public Task<Sediment> SedimentTask { get { return SelectedRangeComplex.SedimentFile.DataTask; } }
        public Task<SoundSpeedField> SoundSpeedFieldTask { get { return CalculateSoundSpeedAsync(); } }
        async Task<SoundSpeedField> CalculateSoundSpeedAsync()
        {
            var dependencies = new List<Task>();
            dependencies.AddRange(_temperatureFiles.Values.Select(item => item.DataTask));
            dependencies.AddRange(_salinityFiles.Values.Select(item => item.DataTask));
            dependencies.Add(_bathymetryFile.DataTask);
            await TaskEx.WhenAll(dependencies);
            var monthlySoundSpeeds = new SoundSpeed();
            foreach (var month in SelectedMonths)
            {
                var temperature = _temperatureFiles[month].DataTask.Result[month];
                var salinity = _salinityFiles[month].DataTask.Result[month];
                monthlySoundSpeeds.SoundSpeedFields.Add(SoundSpeedField.Create(temperature, salinity, Bathymetry.DeepestPoint));
            }
            return SoundSpeed.Average(monthlySoundSpeeds, AvailableTimePeriods[SelectedTimePeriod]);
        }
    }
}
