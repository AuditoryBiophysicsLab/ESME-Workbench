﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.Model;
using ESME.TransmissionLoss.CASS;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    public class NAVODataSources : ViewModelBase
    {
        internal static readonly NAVOTimePeriod[] MonthMap = new[]
        {
                (NAVOTimePeriod)0,
                NAVOTimePeriod.January,
                NAVOTimePeriod.February,
                NAVOTimePeriod.March,
                NAVOTimePeriod.April,
                NAVOTimePeriod.May,
                NAVOTimePeriod.June,
                NAVOTimePeriod.July,
                NAVOTimePeriod.August,
                NAVOTimePeriod.September,
                NAVOTimePeriod.October,
                NAVOTimePeriod.November,
                NAVOTimePeriod.December,
                NAVOTimePeriod.January,
                NAVOTimePeriod.February,
                NAVOTimePeriod.March,
                NAVOTimePeriod.April,
                NAVOTimePeriod.May,
                NAVOTimePeriod.June,
        };

        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;

        //public NAVODataSources(Globals.AppSettings.NAVOConfiguration, _experiment.LocalStorageRoot, _experiment.North, _experiment.South, _experiment.East, _experiment.West, _experiment.NemoFile.Scenario.SimAreaName, _dispatcher);
        public NAVODataSources(GeoRect opArea, NAVOConfiguration configurations, Dispatcher dispatcher, string localStorageRoot, string simAreaPath)
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
            _simAreaPath = simAreaPath;
            _extractionArea = opArea;

            SurfaceMarineGriddedClimatologyDatabase.DatabasePath = configurations.SMGCDirectory;
            SurfaceMarineGriddedClimatologyDatabase.ExtractionProgramPath = Path.Combine(Path.GetDirectoryName(Assembly.GetCallingAssembly().Location), "SMGCExtract.exe");

            DigitalBathymetricDatabase.DatabasePath = configurations.DBDBDirectory;
            DigitalBathymetricDatabase.ExtractionProgramPath = configurations.DBDBEXEPath;
            DigitalBathymetricDatabase = new DigitalBathymetricDatabase();
            DigitalBathymetricDatabase.Initialize();
            DigitalBathymetricDatabase.PropertyChanged += (s, e) => { if (e.PropertyName == "SelectedResolution") UpdateResolutionStatement(); };

            BottomSedimentTypeDatabase.DatabasePath = configurations.BSTDirectory;

            BottomSedimentTypeDatabase = new BottomSedimentTypeDatabase();

            var assemblyLocation = Assembly.GetCallingAssembly().Location;
            if (assemblyLocation == null) throw new ApplicationException("Assembly can't be null!");
            var extractionPath = Path.GetDirectoryName(assemblyLocation);
            if (extractionPath == null) throw new ApplicationException("Extraction path can't be null!");
            ////todo: installer needs to put this in the right place.
            GeneralizedDigitalEnvironmentModelDatabase.ExtractionProgramPath = Path.Combine(extractionPath, "ImportNetCDF.exe");
            GeneralizedDigitalEnvironmentModelDatabase.DatabasePath = configurations.GDEMDirectory;

            UpdateResolutionStatement();
        }

        void UpdateResolutionStatement()
        {
            var resString = DigitalBathymetricDatabase.SelectedResolution.Remove(DigitalBathymetricDatabase.SelectedResolution.Length - 3, 3);
            var resMinutes = double.Parse(resString);
            var samplesPerDegree = 60 / resMinutes;
            BathymetryResolutionStatement = string.Format("Extraction area: {0:0.###}deg (lon) by {1:0.###}deg (lat)\nEstimated point count {2:#,#} x {3:#,#} = {4:#,#}", ExtractionArea.Width, ExtractionArea.Height, ExtractionArea.Width * samplesPerDegree, ExtractionArea.Height * samplesPerDegree, ExtractionArea.Width * ExtractionArea.Height * samplesPerDegree * samplesPerDegree);
            //Console.WriteLine("area {0} {1} {2} {3} 0.10000 sediment-6s.chb", ExpandedExtractionArea.West, ExpandedExtractionArea.East, ExpandedExtractionArea.South, ExpandedExtractionArea.North);
            //Console.WriteLine("area {0} {1} {2} {3} 5.00000 sediment-5m.chb", ExpandedExtractionArea.West, ExpandedExtractionArea.East, ExpandedExtractionArea.South, ExpandedExtractionArea.North);
        }

        #region public GeoRect ExtractionArea { get; set; }

        public GeoRect ExtractionArea
        {
            get { return _extractionArea; }
            set
            {
                if (_extractionArea == value) return;
                _extractionArea = value;
                _expandedExtractionArea = GeoRect.InflateWithGeo(new GeoRect(Math.Ceiling(_extractionArea.North), Math.Floor(_extractionArea.South), Math.Ceiling(_extractionArea.East), Math.Floor(_extractionArea.West)), .01);
                NotifyPropertyChanged(ExtractionAreaChangedEventArgs);
                UpdateResolutionStatement();
            }
        }

        static readonly PropertyChangedEventArgs ExtractionAreaChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.ExtractionArea);
        GeoRect _extractionArea;

        #endregion

        #region public GeoRect ExpandedExtractionArea { get; }

        public GeoRect ExpandedExtractionArea
        {
            get { return _expandedExtractionArea; }
        }

        private GeoRect _expandedExtractionArea;

        #endregion

        #region public string BathymetryResolutionStatement { get; set; }

        public string BathymetryResolutionStatement
        {
            get { return _bathymetryResolutionStatement; }
            set
            {
                if (_bathymetryResolutionStatement == value) return;
                _bathymetryResolutionStatement = value;
                NotifyPropertyChanged(BathymetryResolutionStatementChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs BathymetryResolutionStatementChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.BathymetryResolutionStatement);
        string _bathymetryResolutionStatement;

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

        static readonly PropertyChangedEventArgs UseExpandedExtractionAreaChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.UseExpandedExtractionArea);
        bool _useExpandedExtractionArea;

        #endregion

        public BottomSedimentTypeDatabase BottomSedimentTypeDatabase { get; private set; }
        public DigitalBathymetricDatabase DigitalBathymetricDatabase { get; private set; }

        string _localStorageRoot;
        readonly string _simAreaPath;

        internal NAVOConfiguration Configuration { get; set; }

        void ExtractAreas(object sender, DoWorkEventArgs args)
        {
            var backgroundWorker = (BackgroundWorker) sender;

            var tempDirectory = Path.Combine(_localStorageRoot, "NAVOTemp");

            if (Directory.Exists(tempDirectory))
            {
                Directory.Delete(tempDirectory, true);
                Thread.Sleep(1000);
            }
            Directory.CreateDirectory(tempDirectory);

            var selectedMonthIndices = new List<NAVOTimePeriod>();
            foreach (var timePeriod in SelectedTimePeriods)
                selectedMonthIndices.AddRange(GetMonthIndices(timePeriod));
            var uniqueMonths = selectedMonthIndices.Distinct().ToList();
            uniqueMonths.Sort();

            // uniqueMonths * 3 because we're counting Temp, and Salinity extraction, and Soundspeed creation as independent steps.
            // SelectedTimePeriods.Count() is for averaging the soundspeed fields
            // and the extra 2 is for extracting bathymetry and sediment data which are time invariant
            var averagedSoundSpeedFieldTimePeriods = SelectedTimePeriods.Where(t => GetMonthIndices(t).Count() > 1);
            var totalExtractionStepCount = (float)((uniqueMonths.Count * 3) + averagedSoundSpeedFieldTimePeriods.Count() + 2);
            if (ExportCASSData) totalExtractionStepCount += SelectedTimePeriods.Count();

            totalExtractionStepCount += SelectedTimePeriods.Select(GetMonthIndices).Where(monthIndices => monthIndices.Count() > 1).Count();
            //foreach (var monthIndices in SelectedTimePeriods.Select(GetMonthIndices).Where(monthIndices => monthIndices.Count() > 1))
            //    totalExtractionStepCount++;

            var currentExtractionStep = 0;

            Status = "Extracting bathymetry data for selected area";
            DigitalBathymetricDatabase.ExtractArea(tempDirectory, DigitalBathymetricDatabase.SelectedResolution, ExtractionArea, DigitalBathymetricDatabase.Resolutions);
            if (backgroundWorker.CancellationPending) return;
            ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);
            //var bathymetry = Environment2DData.FromCHB(DigitalBathymetricDatabase.BathymetryCHBFilename(tempDirectory, DigitalBathymetricDatabase.SelectedResolution), -1);
            var bathymetry = Environment2DData.FromYXZ(DigitalBathymetricDatabase.BathymetryYXZFilename(tempDirectory, DigitalBathymetricDatabase.SelectedResolution), -1);
            var maxDepth = new EarthCoordinate<float>(bathymetry.Minimum, Math.Abs(bathymetry.Minimum.Data));

            // BST and DBDB should not need the period to be provided, as these datasets are time-invariant
            Status = "Extracting sediment data for selected area";
            BottomSedimentTypeDatabase.ExtractArea(tempDirectory, ExtractionArea, UseExpandedExtractionArea);
            var sediment = Sediment.Load(BottomSedimentTypeDatabase.SedimentFilename(tempDirectory));
            if (backgroundWorker.CancellationPending) return;
            ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);

            foreach (var month in uniqueMonths)
            {
                Status = "Extracting temperature and salinity data for " + month;
                GeneralizedDigitalEnvironmentModelDatabase.ExtractAreaFromMonthFile(tempDirectory, ExtractionArea, month, UseExpandedExtractionArea);
                if (backgroundWorker.CancellationPending) return;
                currentExtractionStep += 2;
                ProgressPercent = (int)((currentExtractionStep / totalExtractionStepCount) * 100);

                Status = "Calculating sound speed data for " + month;
                GeneralizedDigitalEnvironmentModelDatabase.CreateSoundSpeedFile(tempDirectory, month, maxDepth);
                ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);
                if (backgroundWorker.CancellationPending) return;
            }

            foreach (var timePeriod in averagedSoundSpeedFieldTimePeriods)
            {
                var monthIndices = GetMonthIndices(timePeriod);
                Status = "Calculating average soundspeed data for " + timePeriod;
                GeneralizedDigitalEnvironmentModelDatabase.AverageMonthlyData(tempDirectory, monthIndices, timePeriod);
                if (backgroundWorker.CancellationPending) return;
                ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);
            }

            var soundSpeed = new SoundSpeed();
            foreach (var timePeriod in SelectedTimePeriods)
            {
                var keeperSoundSpeedField = SoundSpeed.Load(SoundspeedFilename(tempDirectory, timePeriod));
                soundSpeed.SoundSpeedFields.Add(keeperSoundSpeedField[timePeriod]);
            }
            soundSpeed.Save(Path.Combine(tempDirectory, "soundspeed.xml"));
            foreach (var soundspeedFile in Directory.GetFiles(tempDirectory, "*-soundspeed.xml"))
                File.Delete(soundspeedFile);


#if true
            Status = "Extracting wind data";
            SurfaceMarineGriddedClimatologyDatabase.ExtractArea(SurfaceMarineGriddedClimatologyDatabase.DatabasePath, tempDirectory, SelectedTimePeriods.ToList(), SelectedTimePeriods.Select(GetMonthIndices).ToList(), ExtractionArea, UseExpandedExtractionArea);
            var wind = Wind.Load(SurfaceMarineGriddedClimatologyDatabase.WindFilename(tempDirectory));
            ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);

#else
            foreach (var timePeriod in SelectedTimePeriods)
            {
                var monthIndices = GetMonthIndices(timePeriod);
                Status = "Extracting wind data for " + timePeriod;
                SurfaceMarineGriddedClimatologyDatabase.ExtractArea(tempDirectory, timePeriod, monthIndices.First(), monthIndices.Last(), monthIndices.Count(), selectedExtractionArea);
                if (backgroundWorker.CancellationPending) return;
                ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);
            }
#endif

            if (ExportCASSData)
            {
                Status = "Exporting bathymetry data";
                //var bathymetryFileName = Path.Combine(Path.Combine(_simAreaPath, "Bathymetry"), "bathy_" + DigitalBathymetricDatabase.SelectedResolution + ".txt");
                var cassBathymetryFile = Path.Combine(Path.Combine(_simAreaPath, "Bathymetry"), "bathymetry.txt");
                File.Copy(DigitalBathymetricDatabase.BathymetryYXZFilename(tempDirectory, DigitalBathymetricDatabase.SelectedResolution), cassBathymetryFile, true);
                //CASSFiles.WriteBathymetryFile(cassBathymetryFile, bathymetry);

                foreach (var timePeriod in SelectedTimePeriods)
                {
                    Status = "Exporting CASS format data for " + timePeriod;

                    CASSFiles.GenerateSimAreaData(_simAreaPath, tempDirectory, timePeriod, bathymetry, sediment, soundSpeed[timePeriod], wind[timePeriod].EnvironmentData);
                    if (backgroundWorker.CancellationPending) return;
                    ProgressPercent = (int)((++currentExtractionStep / totalExtractionStepCount) * 100);
                }
            }

            // At this point, the user can no longer cancel the operation.
            var environmentDirectory = Path.Combine(_localStorageRoot, "Environment");
            if (!Directory.Exists(environmentDirectory)) Directory.CreateDirectory(environmentDirectory);
            var files = Directory.GetFiles(environmentDirectory);
            foreach (var file in files) File.Delete(file);
            files = Directory.GetFiles(tempDirectory);
            foreach (var sourceFile in files)
            {
                var destFile = Path.Combine(environmentDirectory, Path.GetFileName(sourceFile));
                File.Move(sourceFile, destFile);
            }
            Directory.Delete(tempDirectory, true);

            _localStorageRoot = environmentDirectory;
        }

        public IEnumerable<NAVOTimePeriod> SelectedTimePeriods { get; set; }

        public string TemperatureFilename(NAVOTimePeriod timePeriod) { return GeneralizedDigitalEnvironmentModelDatabase.TemperatureFilename(_localStorageRoot, timePeriod); }
        public string SalinityFilename(NAVOTimePeriod timePeriod) { return GeneralizedDigitalEnvironmentModelDatabase.SalinityFilename(_localStorageRoot, timePeriod); }
        public string SoundspeedFilename(string directoryName, NAVOTimePeriod timePeriod) { return GeneralizedDigitalEnvironmentModelDatabase.SoundspeedFilename(directoryName, timePeriod); }
        public string WindFilename { get { return SurfaceMarineGriddedClimatologyDatabase.WindFilename(_localStorageRoot); } }
        public string SedimentFilename { get { return BottomSedimentTypeDatabase.SedimentFilename(_localStorageRoot); } }
        public string BathymetryFilename { get { return DigitalBathymetricDatabase.BathymetryYXZFilename(_localStorageRoot, DigitalBathymetricDatabase.SelectedResolution); } }

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
                _isStarted = value;
                IsVisible = _isStarted ? Visibility.Visible : Visibility.Collapsed;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(IsStartedChangedEventArgs));
            }
        }

        static readonly PropertyChangedEventArgs IsStartedChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.IsStarted);
        bool _isStarted;

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
                ProgressPercent = 0;
                _backgroundWorker = new BackgroundWorker
                                    {
                                        WorkerSupportsCancellation = true,
                                        WorkerReportsProgress = true,
                                    };
                _backgroundWorker.DoWork += ExtractAreas;
                _backgroundWorker.RunWorkerCompleted += (s, e) =>
                                                        {
                                                            IsStarted = false;
                                                            ProgressPercent = 100;
                                                            if (e.Cancelled) Status = "Canceled";
                                                            else if (e.Error != null)
                                                            {
                                                                Status = "Error";
                                                                throw new ApplicationException(string.Format("Error extracting environmental data"), e.Error);
                                                            }
                                                        };
                if (runWorkerCompletedEventHandler != null) _backgroundWorker.RunWorkerCompleted += runWorkerCompletedEventHandler;
                _backgroundWorker.RunWorkerAsync();
            }
        }

        static readonly PropertyChangedEventArgs StatusChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.Status);
        string _status;

        #endregion

        #region public bool ExportCASSData { get; set; }

        public bool ExportCASSData
        {
            get { return _exportCASSData; }
            set
            {
                if (_exportCASSData == value) return;
                _exportCASSData = value;
                NotifyPropertyChanged(ExportCASSDataChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs ExportCASSDataChangedEventArgs = ObservableHelper.CreateArgs<NAVODataSources>(x => x.ExportCASSData);
        bool _exportCASSData;

        #endregion

        public IEnumerable<NAVOTimePeriod> GetMonthIndices(NAVOTimePeriod timePeriod)
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
                    yield return timePeriod;
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