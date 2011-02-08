using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.TransmissionLoss.CASS;

namespace ESME.Environment.NAVO
{
    public class NAVODataSources : ViewModelBase
    {
        internal static readonly int[] MonthMap = new[]
                                                  {
                                                      0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6
                                                  };

        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;

        //public NAVODataSources(Globals.AppSettings.NAVOConfiguration, _experiment.LocalStorageRoot, _experiment.North, _experiment.South, _experiment.East, _experiment.West, _experiment.NemoFile.Scenario.SimAreaName, _dispatcher);
        public NAVODataSources(NAVOConfiguration configurations, Dispatcher dispatcher, string localStorageRoot, float north, float south, float east, float west, string simAreaName)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                Debug.WriteLine("***********\nNAVODataSources: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            Configuration = configurations;
            _dispatcher = dispatcher;
            _localStorageRoot = localStorageRoot;
            _north = north;
            _south = south;
            _east = east;
            _west = west;
            _simAreaName = simAreaName;

            SurfaceMarineGriddedClimatologyDatabase.DatabasePath = configurations.SMGCDirectory;
            SurfaceMarineGriddedClimatologyDatabase.ExtractionProgramPath = configurations.SMGCEXEPath;

            DigitalBathymetricDatabase.DatabasePath = configurations.DBDBDirectory;
            DigitalBathymetricDatabase.ExtractionProgramPath = configurations.DBDBEXEPath;
            DigitalBathymetricDatabase = new DigitalBathymetricDatabase();
            DigitalBathymetricDatabase.Initialize();

            BottomSedimentTypeDatabase.DatabasePath = configurations.BSTDirectory;
            BottomSedimentTypeDatabase.ExtractionProgramPath = configurations.BSTEXEPath;

            BottomSedimentTypeDatabase = new BottomSedimentTypeDatabase();
            BottomSedimentTypeDatabase.Initialize();

            var assemblyLocation = Assembly.GetCallingAssembly().Location;
            if (assemblyLocation == null) throw new ApplicationException("Assembly can't be null!");
            var extractionPath = Path.GetDirectoryName(assemblyLocation);
            if (extractionPath == null) throw new ApplicationException("Extraction path can't be null!");
            ////todo: installer needs to put this in the right place.
            GeneralizedDigitalEnvironmentModelDatabase.ExtractionProgramPath = Path.Combine(extractionPath, "ImportNetCDF.exe");
            GeneralizedDigitalEnvironmentModelDatabase.DatabasePath = configurations.GDEMDirectory;
        }

        public BottomSedimentTypeDatabase BottomSedimentTypeDatabase { get; private set; }
        public DigitalBathymetricDatabase DigitalBathymetricDatabase { get; private set; }

        readonly string _localStorageRoot;
        readonly float _north;
        readonly float _south;
        readonly float _east;
        readonly float _west;
        readonly string _simAreaName;

        internal NAVOConfiguration Configuration { get; set; }

        void ExtractAreas(object sender, DoWorkEventArgs args)
        {
            var backgroundWorker = (BackgroundWorker) sender;

            var selectedMonthIndices = new List<int>();
            foreach (var timePeriod in SelectedTimePeriods)
                selectedMonthIndices.AddRange(GetMonthIndices(timePeriod));
            var uniqueMonths = selectedMonthIndices.Distinct().ToList();
            uniqueMonths.Sort();

            var totalExtractionStepCount = (float)(uniqueMonths.Count + (SelectedTimePeriods.Count() * 2) + 2);
            foreach (var monthIndices in SelectedTimePeriods.Select(GetMonthIndices).Where(monthIndices => monthIndices.Count() > 1)) 
                totalExtractionStepCount++;

            var currentExtractionStep = 0;

            foreach (var month in uniqueMonths)
            {
                Status = "Extracting temperature and salinity data for " + (NAVOTimePeriod)month;
                GeneralizedDigitalEnvironmentModelDatabase.ExtractAreaFromMonthFile(_localStorageRoot, _north, _south, _east, _west, month);
                if (backgroundWorker.CancellationPending) return;
                ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);
            }

            foreach (var timePeriod in SelectedTimePeriods)
            {
                var monthIndices = GetMonthIndices(timePeriod);
                if (monthIndices.Count() <= 1) continue;
                Status = "Creating average temperature and salinity data for " + timePeriod;
                GeneralizedDigitalEnvironmentModelDatabase.AverageMonthlyData(_localStorageRoot, monthIndices, timePeriod);
                if (backgroundWorker.CancellationPending) return;
                ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);
            }

            foreach (var timePeriod in SelectedTimePeriods)
            {
                Status = "Creating sound speed data for " + timePeriod;
                GeneralizedDigitalEnvironmentModelDatabase.CreateSoundSpeedFile(_localStorageRoot, timePeriod);
                if (backgroundWorker.CancellationPending) return;
                ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);
            }

            // BST and DBDB should not need the period to be provided, as these datasets are time-invariant
            Status = "Extracting sediment data for selected area";
            BottomSedimentTypeDatabase.ExtractArea(_localStorageRoot, BottomSedimentTypeDatabase.SelectedResolution, _north, _south, _east, _west);
            if (backgroundWorker.CancellationPending) return;
            ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);

            Status = "Extracting bathymetry data for selected area";
            DigitalBathymetricDatabase.ExtractArea(_localStorageRoot, DigitalBathymetricDatabase.SelectedResolution, _north, _south, _east, _west, DigitalBathymetricDatabase.Resolutions);
            if (backgroundWorker.CancellationPending) return;
            ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);

            foreach (var timePeriod in SelectedTimePeriods)
            {
                var monthIndices = GetMonthIndices(timePeriod);
                Status = "Extracting wind data for " + timePeriod;
                SurfaceMarineGriddedClimatologyDatabase.ExtractArea(_localStorageRoot, timePeriod, monthIndices.First(), monthIndices.Last(), monthIndices.Count(), _north, _south, _east, _west);
                if (backgroundWorker.CancellationPending) return;
                ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);
            }

            CASSFiles.GenerateSimAreaData(_simAreaName, _localStorageRoot, _north, _south, _east, _west);
        }

        public IEnumerable<NAVOTimePeriod> SelectedTimePeriods { get; set; }

        #region public int ProgressPercent { get; set; }

        public int ProgressPercent
        {
            get { return _progressPercent; }
            set
            {
                if (_progressPercent == value) return;
                _progressPercent = value;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(ProgressPercentChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs ProgressPercentChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.ProgressPercent);
        int _progressPercent;

        #endregion

        #region public Visibility IsVisible { get; set; }

        public Visibility IsVisible
        {
            get { return _isVisible; }
            set
            {
                if (_isVisible == value) return;
                _isVisible = value;
                NotifyPropertyChanged(IsVisibleChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsVisibleChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.IsVisible);
        Visibility _isVisible = Visibility.Collapsed;

        #endregion

        #region public bool IsStarted { get; set; }

        public bool IsStarted
        {
            get { return _isStarted; }
            set
            {
                if (_isStarted == value) return;
                _isStarted = value;
                IsVisible = _isStarted ? Visibility.Visible : Visibility.Collapsed;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(IsStartedChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs IsStartedChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.IsStarted);
        bool _isStarted;

        #endregion

        #region public bool IsCompleted { get; set; }

        public bool IsCompleted
        {
            get { return _isCompleted; }
            set
            {
                if (_isCompleted == value) return;
                _isCompleted = value;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(IsCompletedChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs IsCompletedChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.IsCompleted);
        bool _isCompleted;

        #endregion

        #region public string Status { get; set; }

        public string Status
        {
            get { return _status; }
            set
            {
                if (_status == value) return;
                _status = value;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(StatusChangedEventArgs));
            }
        }

        public void CancelExtraction()
        {
            if (_backgroundWorker.IsBusy)
            {
                _backgroundWorker.CancelAsync();
                Status = "Canceling, please wait...";
            }
        }

        BackgroundWorker _backgroundWorker;
        public void ExtractDataInBackground(RunWorkerCompletedEventHandler runWorkerCompletedEventHandler)
        {
            lock (this)
            {
                if (IsStarted) return;
                IsStarted = true;
                _backgroundWorker = new BackgroundWorker
                                    {
                                        WorkerSupportsCancellation = true,
                                        WorkerReportsProgress = true,
                                    };
                _backgroundWorker.DoWork += ExtractAreas;
                _backgroundWorker.RunWorkerCompleted += delegate
                                                        {
                                                            IsCompleted = true;
                                                            ProgressPercent = 0;
                                                        };
                if (runWorkerCompletedEventHandler != null) _backgroundWorker.RunWorkerCompleted += runWorkerCompletedEventHandler;
                _backgroundWorker.RunWorkerAsync();
            }
        }

        static readonly PropertyChangedEventArgs StatusChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.Status);
        string _status;

        #endregion

        IEnumerable<int> GetMonthIndices(NAVOTimePeriod timePeriod)
        {
            switch (timePeriod)
            {
                case NAVOTimePeriod.January:
                case NAVOTimePeriod.February:
                case NAVOTimePeriod.March:
                case NAVOTimePeriod.April:
                case NAVOTimePeriod.May:
                case NAVOTimePeriod.June:
                case NAVOTimePeriod.July:
                case NAVOTimePeriod.August:
                case NAVOTimePeriod.September:
                case NAVOTimePeriod.October:
                case NAVOTimePeriod.November:
                case NAVOTimePeriod.December:
                    yield return (int) timePeriod;
                    yield break;
                case NAVOTimePeriod.Spring:
                    yield return MonthMap[(int) Configuration.SpringStartMonth];
                    yield return MonthMap[(int) Configuration.SpringStartMonth + 1];
                    yield return MonthMap[(int) Configuration.SpringStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Summer:
                    yield return MonthMap[(int) Configuration.SummerStartMonth];
                    yield return MonthMap[(int) Configuration.SummerStartMonth + 1];
                    yield return MonthMap[(int) Configuration.SummerStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Fall:
                    yield return MonthMap[(int) Configuration.FallStartMonth];
                    yield return MonthMap[(int) Configuration.FallStartMonth + 1];
                    yield return MonthMap[(int) Configuration.FallStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Winter:
                    yield return MonthMap[(int) Configuration.WinterStartMonth];
                    yield return MonthMap[(int) Configuration.WinterStartMonth + 1];
                    yield return MonthMap[(int) Configuration.WinterStartMonth + 2];
                    yield break;
                case NAVOTimePeriod.Cold:
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth];
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth + 1];
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth + 2];
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth + 3];
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth + 4];
                    yield return MonthMap[(int) Configuration.ColdSeasonStartMonth + 5];
                    yield break;
                case NAVOTimePeriod.Warm:
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth];
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth + 1];
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth + 2];
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth + 3];
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth + 4];
                    yield return MonthMap[(int) Configuration.WarmSeasonStartMonth + 5];
                    yield break;
            }
        }

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
        }
    }
}