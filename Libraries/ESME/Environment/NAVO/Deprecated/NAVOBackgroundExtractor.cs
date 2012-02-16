using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using Cinch;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.NAVO
{
#if false
    public abstract class NAVOBackgroundExtractor : BackgroundTask
    {
        #region public GeoRect ExtractionArea { get; set; }

        public GeoRect ExtractionArea
        {
            get { return _extractionArea; }
            set
            {
                if (_extractionArea == value) return;
                _extractionArea = value;
                NotifyPropertyChanged(ExtractionAreaChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExtractionAreaChangedEventArgs = ObservableHelper.CreateArgs<NAVOBackgroundExtractor>(x => x.ExtractionArea);
        GeoRect _extractionArea;

        #endregion

        #region public List<TimePeriod> SelectedTimePeriods { get; set; }

        public List<NAVOTimePeriod> SelectedTimePeriods
        {
            get { return _selectedTimePeriods; }
            set
            {
                if (_selectedTimePeriods == value) return;
                _selectedTimePeriods = value;
                NotifyPropertyChanged(SelectedTimePeriodsChangedEventArgs);

                UniqueMonths.Clear();
                var selectedMonthIndices = new List<NAVOTimePeriod>();
                foreach (var timePeriod in _selectedTimePeriods)
                    selectedMonthIndices.AddRange(Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(timePeriod));
                UniqueMonths.AddRange(selectedMonthIndices.Distinct());
                UniqueMonths.Sort();
                NotifyPropertyChanged(UniqueMonthsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedTimePeriodsChangedEventArgs = ObservableHelper.CreateArgs<NAVOBackgroundExtractor>(x => x.SelectedTimePeriods);
        List<NAVOTimePeriod> _selectedTimePeriods;

        #endregion

        #region public NAVOConfiguration NAVOConfiguration { get; set; }

        public NAVOConfiguration NAVOConfiguration
        {
            get { return _navoConfiguration; }
            set
            {
                if (_navoConfiguration == value) return;
                _navoConfiguration = value;
                NotifyPropertyChanged(NAVOConfigurationChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs NAVOConfigurationChangedEventArgs = ObservableHelper.CreateArgs<NAVOBackgroundExtractor>(x => x.NAVOConfiguration);
        NAVOConfiguration _navoConfiguration;

        #endregion

        #region public TimePeriod TimePeriod { get; set; }

        public NAVOTimePeriod TimePeriod
        {
            get { return _timePeriod; }
            set
            {
                if (_timePeriod == value) return;
                _timePeriod = value;
                NotifyPropertyChanged(TimePeriodChangedEventArgs);

                UniqueMonths.Clear();
                UniqueMonths.AddRange(Globals.AppSettings.NAVOConfiguration.MonthsInTimePeriod(_timePeriod));
                UniqueMonths.Sort();
                NotifyPropertyChanged(UniqueMonthsChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs TimePeriodChangedEventArgs = ObservableHelper.CreateArgs<NAVOBackgroundExtractor>(x => x.TimePeriod);
        NAVOTimePeriod _timePeriod;

        #endregion

        #region public List<TimePeriod> UniqueMonths { get; set; }

        public List<NAVOTimePeriod> UniqueMonths
        {
            get { return _uniqueMonths ?? (_uniqueMonths = new List<NAVOTimePeriod>()); }
        }

        static readonly PropertyChangedEventArgs UniqueMonthsChangedEventArgs = ObservableHelper.CreateArgs<NAVOBackgroundExtractor>(x => x.UniqueMonths);
        List<NAVOTimePeriod> _uniqueMonths;

        #endregion

        #region public string DestinationPath { get; set; }

        public string DestinationPath
        {
            get { return _destinationPath; }
            set
            {
                if (_destinationPath == value) return;
                _destinationPath = value;
                NotifyPropertyChanged(SimAreaPathChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SimAreaPathChangedEventArgs = ObservableHelper.CreateArgs<NAVOBackgroundExtractor>(x => x.DestinationPath);
        string _destinationPath;

        #endregion

        #region public bool UseExpandedExtractionArea { get; set; }

        public bool UseExpandedExtractionArea
        {
            get { return _useExpandedExtractionArea; }
            set
            {
                if (_useExpandedExtractionArea == value) return;
                _useExpandedExtractionArea = value;
                NotifyPropertyChanged(UseExpandedExtractionAreaChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs UseExpandedExtractionAreaChangedEventArgs = ObservableHelper.CreateArgs<NAVOBackgroundExtractor>(x => x.UseExpandedExtractionArea);
        bool _useExpandedExtractionArea;

        #endregion

        #region public float SelectedResolution { get; set; }

        public float SelectedResolution
        {
            get { return _selectedResolution; }
            set
            {
                if (_selectedResolution == value) return;
                _selectedResolution = value;
                NotifyPropertyChanged(SelectedResolutionChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SelectedResolutionChangedEventArgs = ObservableHelper.CreateArgs<NAVOBackgroundExtractor>(x => x.SelectedResolution);
        float _selectedResolution;

        #endregion

        #region public string SaveAsFilename { get; set; }

        public string SaveAsFilename
        {
            get { return _saveAsFilename; }
            set
            {
                if (_saveAsFilename == value) return;
                _saveAsFilename = value;
                if (_saveAsFilename != null) _saveAsFilename = _saveAsFilename.Replace(' ', '_');
                NotifyPropertyChanged(SaveAsFilenameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SaveAsFilenameChangedEventArgs = ObservableHelper.CreateArgs<NAVOBackgroundExtractor>(x => x.SaveAsFilename);
        string _saveAsFilename;

        #endregion
    }
#endif
}
