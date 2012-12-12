using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel.Composition;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Threading;
using ESME.SimulationAnalysis;
using ESME.Simulator;
using ESME.Views.Simulation;
using HRC;
using HRC.Aspects;
using HRC.Services;
using HRC.Validation;
using HRC.ViewModels;
using HRC.WPF;
using MEFedMVVM.ViewModelLocator;

namespace SimulationLogAnalysis
{
    [ExportViewModel("SimulationLogAnalysisMainViewModel")]
    public class SimulationLogAnalysisMainViewModel : ValidatingViewModel, IHistogramSource
    {
        readonly IViewAwareStatus _viewAwareStatus;
        readonly IMessageBoxService _messageBox;
        readonly IUIVisualizerService _visualizer;
        readonly IHRCSaveFileService _saveFile;
        [UsedImplicitly] readonly PropertyObserver<SimulationLogAnalysisMainViewModel> _propertyObserver;
        [UsedImplicitly] CollectionObserver _modeBinsCollectionObserver;
        Dispatcher _dispatcher;
        const string TimeSpanFormatString = @"hh\:mm\:ss";
        TimeSpan _simulationStartTime, _simulationEndTime;
        readonly ValidationRule<SimulationLogAnalysisMainViewModel> _startTimeValidationRule;
        readonly ValidationRule<SimulationLogAnalysisMainViewModel> _endTimeValidationRule;

        [ImportingConstructor]
        public SimulationLogAnalysisMainViewModel(IViewAwareStatus viewAwareStatus,
                                                  IMessageBoxService messageBox,
                                                  IUIVisualizerService visualizer,
                                                  IHRCSaveFileService saveFile)
        {
            _viewAwareStatus = viewAwareStatus;
            _messageBox = messageBox;
            _visualizer = visualizer;
            _saveFile = saveFile;
            _simulationStartTime = new TimeSpan(0);
            _simulationEndTime = new TimeSpan(0);
            _viewAwareStatus.ViewLoaded += () =>
            {
                _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
                SelectedFileName = (string)Application.Current.Properties["SelectedFileName"];
            };
            _propertyObserver = new PropertyObserver<SimulationLogAnalysisMainViewModel>(this)
                .RegisterHandler(p => p.SelectedFileName, SelectedFileNameChanged)
                .RegisterHandler(p => p.AreAnyPlatformsSelected, CommandManager.InvalidateRequerySuggested)
                .RegisterHandler(p => p.AreAnyModesSelected, CommandManager.InvalidateRequerySuggested)
                .RegisterHandler(p => p.AreAnySpeciesSelected, CommandManager.InvalidateRequerySuggested)
                .RegisterHandler(p => p.PerformOurOnlyAnalysis, CommandManager.InvalidateRequerySuggested)
                .RegisterHandler(p => p.StartTimeString, StartOrStopTimeStringsChanged)
                .RegisterHandler(p => p.StopTimeString, StartOrStopTimeStringsChanged)
                .RegisterHandler(p => p.AllTimes,
                                 () =>
                                 {
                                     if (!AllTimes || SelectedFileName == null || !File.Exists(SelectedFileName)) return;
                                     _filterStartTime = new TimeSpan(_simulationStartTime.Ticks);
                                     _filterEndTime = new TimeSpan(_simulationEndTime.Ticks);
                                     StartTimeString = _filterStartTime.ToString(TimeSpanFormatString);
                                     StopTimeString = _filterEndTime.ToString(TimeSpanFormatString);
                                 });
            AddValidationRules(new ValidationRule<SimulationLogAnalysisMainViewModel>
            {
                PropertyName = "StartTimeString",
                Description = "Must be a valid, non-negative time span value in the format hh:mm:ss where 00 <= hh <= 23; 00 <= mm <= 59; 00 <= ss <= 59",
                IsRuleValid = (target, rule) =>
                {
                    if (AllTimes) return true;
                    if (string.IsNullOrEmpty(target.StartTimeString)) return false;
                    TimeSpan timeSpan;
                    var isOK = TimeSpan.TryParseExact(target.StartTimeString, TimeSpanFormatString, null, out timeSpan);
                    return isOK && timeSpan.Ticks >= 0;
                },
            }, new ValidationRule<SimulationLogAnalysisMainViewModel>
            {
                PropertyName = "StopTimeString",
                Description = "Must be a valid, non-negative time span value in the format hh:mm:ss where 00 <= hh <= 23; 00 <= mm <= 59; 00 <= ss <= 59",
                IsRuleValid = (target, rule) =>
                {
                    if (AllTimes) return true;
                    if (string.IsNullOrEmpty(target.StopTimeString)) return false;
                    TimeSpan timeSpan;
                    var isOK = TimeSpan.TryParseExact(target.StopTimeString, TimeSpanFormatString, null, out timeSpan);
                    return isOK && timeSpan.Ticks > 0;
                },
            });
            _startTimeValidationRule = new ValidationRule<SimulationLogAnalysisMainViewModel>
            {
                PropertyName = "StartTimeString",
                Description = string.Format("Must be between {0} and {1}", _simulationStartTime.ToString(TimeSpanFormatString), _filterEndTime.ToString(TimeSpanFormatString)),
                IsRuleValid = (target, rule) =>
                {
                    if (AllTimes) return true;
                    if (_simulationStartTime.Ticks == 0 && _simulationEndTime.Ticks == 0) return true;
                    if (string.IsNullOrEmpty(target.StartTimeString)) return false;
                    TimeSpan timeSpan;
                    var isOK = TimeSpan.TryParseExact(target.StartTimeString, TimeSpanFormatString, null, out timeSpan);
                    return isOK && timeSpan >= _simulationStartTime && timeSpan < _filterEndTime;
                },
            };
            _endTimeValidationRule = new ValidationRule<SimulationLogAnalysisMainViewModel>
            {
                PropertyName = "StopTimeString",
                Description = string.Format("Must be between {0} and {1}", _filterStartTime.ToString(TimeSpanFormatString), _simulationEndTime.ToString(TimeSpanFormatString)),
                IsRuleValid = (target, rule) =>
                {
                    if (AllTimes) return true;
                    if (_simulationStartTime.Ticks == 0 && _simulationEndTime.Ticks == 0) return true;
                    if (string.IsNullOrEmpty(target.StopTimeString)) return false;
                    TimeSpan timeSpan;
                    var isOK = TimeSpan.TryParseExact(target.StopTimeString, TimeSpanFormatString, null, out timeSpan);
                    return isOK && timeSpan > _filterStartTime && timeSpan <= _simulationEndTime;
                },
            };
            AddValidationRules(_startTimeValidationRule, _endTimeValidationRule);
        }

        void StartOrStopTimeStringsChanged() 
        {
            TimeSpan timeSpan;
            var isOK = TimeSpan.TryParseExact(StartTimeString, TimeSpanFormatString, null, out timeSpan);
            if (isOK) _filterStartTime = timeSpan;
            isOK = TimeSpan.TryParseExact(StopTimeString, TimeSpanFormatString, null, out timeSpan);
            if (isOK) _filterEndTime = timeSpan;
        }

        void UpdateHistogramDisplay()
        {
            var handlers = GraphicsUpdate;
            if (handlers == null) return;
            foreach (EventHandler<EventArgs> handler in handlers.GetInvocationList())
            {
                if (handler.Target is DispatcherObject)
                {
                    var localHandler = handler;
                    ((DispatcherObject)handler.Target).Dispatcher.InvokeIfRequired(() => localHandler(this, new EventArgs()));
                }
                else
                    handler(this, new EventArgs());
            }
        }

        void SelectedFileNameChanged()
        {
            ResetSelectionObservers();
            AvailablePlatforms.Clear();
            AvailableModes.Clear();
            AvailableSpecies.Clear();
            GuidToColorMap.Clear();
            foreach (var window in OpenWindows) window.Close();
            OpenWindows.Clear();
            if (SimulationLog != null) SimulationLog.Close();
            _simulationStartTime = new TimeSpan(0, 0, 0, 0);
            _simulationEndTime = new TimeSpan(0, 0, 0, 0);
            IsLogFileSelected = false;
            if (SelectedFileName == null || !File.Exists(SelectedFileName)) return;
            IsLogFileSelected = true;
            SimulationLog = SimulationLog.Open(SelectedFileName);
            StartTimeString = _simulationStartTime.ToString(TimeSpanFormatString);
            _simulationEndTime = new TimeSpan(SimulationLog.TimeStepSize.Ticks * SimulationLog.TimeStepCount);
            StopTimeString = _simulationEndTime.ToString(TimeSpanFormatString);
            _startTimeValidationRule.Description = string.Format("Must be between {0} and {1}", _simulationStartTime.ToString(TimeSpanFormatString), _simulationEndTime.ToString(TimeSpanFormatString));
            _endTimeValidationRule.Description = string.Format("Must be between {0} and {1}", _simulationStartTime.ToString(TimeSpanFormatString), _simulationEndTime.ToString(TimeSpanFormatString));

            // todo: Populate platform, mode and species lists
            foreach (var modeFilter in SimulationLog.ModeRecords.Select(mode => new ContentFilterRecordBase(mode) { Name = string.Format("{0}:{1}", mode.PlatformRecord.Name, mode.Name) })) 
            {
                _modeSelectionObservers.Add(new PropertyObserver<ContentFilterRecordBase>(modeFilter).RegisterHandler(p => p.IsSelected, ModeFilterSelectionChanged));
                AvailableModes.Add(modeFilter);
            }
            ModeFilterSelectionChanged();
            foreach (var platform in SimulationLog.PlatformRecords)
            {
                var curPlatform = platform;
                var dependentModes = from mode in AvailableModes
                                     where ((ModeNameGuid)mode.NameGuidRecord).PlatformGuid == curPlatform.Guid
                                     select mode;
                var platformFilter = new PlatformContentFilterRecord(curPlatform, dependentModes);
                _platformSelectionObservers.Add(new PropertyObserver<ContentFilterRecordBase>(platformFilter).RegisterHandler(p => p.IsSelected, PlatformFilterSelectionChanged)); 
                AvailablePlatforms.Add(platformFilter);
            }
            PlatformFilterSelectionChanged();
            foreach (var speciesFilter in SimulationLog.SpeciesRecords.Select(species => new ContentFilterRecordBase(species))) 
            {
                _speciesSelectionObservers.Add(new PropertyObserver<ContentFilterRecordBase>(speciesFilter).RegisterHandler(p => p.IsSelected, SpeciesFilterSelectionChanged));
                AvailableSpecies.Add(speciesFilter);
            }
            SpeciesFilterSelectionChanged();
            for (var speciesIndex = 0; speciesIndex < SimulationLog.SpeciesRecords.Count; speciesIndex++) GuidToColorMap.Add(SimulationLog.SpeciesRecords[speciesIndex].Guid, _barColors[speciesIndex % _barColors.Count]);
            CommandManager.InvalidateRequerySuggested();
        }

        void PlatformFilterSelectionChanged() { AreAnyPlatformsSelected = (from p in AvailablePlatforms where p.IsSelected select p).Any(); }
        void ModeFilterSelectionChanged() { AreAnyModesSelected = (from m in AvailableModes where m.IsSelected select m).Any(); }
        void SpeciesFilterSelectionChanged() { AreAnySpeciesSelected = (from s in AvailableSpecies where s.IsSelected select s).Any(); }

        void ResetSelectionObservers()
        {
            foreach (var observer in _platformSelectionObservers) observer.UnregisterHandler(p => p.IsSelected);
            foreach (var observer in _modeSelectionObservers) observer.UnregisterHandler(p => p.IsSelected);
            foreach (var observer in _speciesSelectionObservers) observer.UnregisterHandler(p => p.IsSelected);
            _platformSelectionObservers.Clear();
            _modeSelectionObservers.Clear();
            _speciesSelectionObservers.Clear();
        }

        public bool IsLogFileSelected { get; private set; }
        public string SelectedFileName { get; set; }
        [Initialize(true)] public bool AllTimes { get; set; }
        [Initialize(false)] public bool SelectedTimeRange { get; set; }
        public string StartTimeString { get; set; }
        public string StopTimeString { get; set; }
        TimeSpan _filterStartTime, _filterEndTime;

        [Initialize(100.0)] public double StartBinValue { get; set; }
        [Initialize(10.0)] public double BinWidth { get; set; }
        [Initialize(10)] public int BinCount { get; set; }

        [Initialize(true)] public bool PerformOurOnlyAnalysis { get; set; }
        public SimulationLog SimulationLog { get; set; }
        [Initialize, UsedImplicitly] List<Window> OpenWindows { get; set; }
        #region AnalyzeCommand
        public SimpleCommand<object, EventToCommandArgs> AnalyzeCommand { get { return _analyze ?? (_analyze = new SimpleCommand<object, EventToCommandArgs>(o => IsAnalyzeCommandEnabled, AnalyzeHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _analyze;

        bool IsAnalyzeCommandEnabled { get { return PerformOurOnlyAnalysis && AreAnyPlatformsSelected && AreAnyModesSelected && AreAnySpeciesSelected; } }
        void AnalyzeHandler(EventToCommandArgs args) { TaskEx.Run(PerformAnalysis); }
        #endregion

        #region CloseCommand
        public SimpleCommand<object, EventToCommandArgs> CloseCommand { get { return _close ?? (_close = new SimpleCommand<object, EventToCommandArgs>(CloseHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _close;

        void CloseHandler(EventToCommandArgs args)
        {
            foreach (var window in OpenWindows) window.Close();
            Application.Current.Shutdown();
        }
        #endregion

        #region SelectAllPlatformsCommand
        public SimpleCommand<object, EventToCommandArgs> SelectAllPlatformsCommand { get { return _selectAllPlatforms ?? (_selectAllPlatforms = new SimpleCommand<object, EventToCommandArgs>(SelectAllPlatformsHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _selectAllPlatforms;

        void SelectAllPlatformsHandler(EventToCommandArgs args) { foreach (var platform in AvailablePlatforms) platform.IsSelected = true; }
        #endregion

        #region SelectAllModesCommand
        public SimpleCommand<object, EventToCommandArgs> SelectAllModesCommand { get { return _selectAllModes ?? (_selectAllModes = new SimpleCommand<object, EventToCommandArgs>(SelectAllModesHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _selectAllModes;

        void SelectAllModesHandler(EventToCommandArgs args) { foreach (var mode in AvailableModes) mode.IsSelected = true; }
        #endregion

        #region SelectAllSpeciesCommand
        public SimpleCommand<object, EventToCommandArgs> SelectAllSpeciesCommand { get { return _selectAllSpecies ?? (_selectAllSpecies = new SimpleCommand<object, EventToCommandArgs>(SelectAllSpeciesHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _selectAllSpecies;

        void SelectAllSpeciesHandler(EventToCommandArgs args) { foreach (var species in AvailableSpecies) species.IsSelected = true; }
        #endregion

        public bool AreAnyPlatformsSelected { get; private set; }
        public bool AreAnyModesSelected { get; private set; }
        public bool AreAnySpeciesSelected { get; private set; }
        async void PerformAnalysis()
        {
            Task<bool> processTask = null;
            var selectedModeIds = (from mode in AvailableModes
                                   where mode.IsSelected
                                   select ((ModeNameGuid)mode.NameGuidRecord).ActorID).ToList();
            var selectedSpeciesGuids = (from species in AvailableSpecies
                                        where species.IsSelected
                                        select species.NameGuidRecord.Guid).ToList();
            Func<ActorExposureRecord, bool> modeFilter = record =>
            {
                var speciesRecord = SimulationLog.RecordFromActorID(record.ActorID) as SpeciesNameGuid;
                return speciesRecord != null && selectedModeIds.Contains(record.ModeID);
            };
            Func<ActorExposureRecord, bool> speciesFilter = record =>
            {
                var speciesRecord = SimulationLog.RecordFromActorID(record.ActorID) as SpeciesNameGuid;
                return speciesRecord != null && selectedSpeciesGuids.Contains(speciesRecord.Guid);
            };
            var histogramBinsViewModels = new ObservableCollection<HistogramBinsViewModel>();

            _dispatcher.InvokeIfRequired(() => OpenWindows.Add(_visualizer.ShowWindow("SimulationExposuresView", new SimulationExposuresViewModel(histogramBinsViewModels))));
            var modeThresholdHistogram = new ModeThresholdHistogram(this, SimulationLog, StartBinValue, BinWidth, BinCount, modeFilter, speciesFilter);
            _modeBinsCollectionObserver = new CollectionObserver(modeThresholdHistogram.GroupedExposures.Groups)
                .RegisterHandler((s, e) =>
                {
                    switch (e.Action)
                    {
                        case NotifyCollectionChangedAction.Add:
                            foreach (GroupedExposuresHistogram histogram in e.NewItems) histogramBinsViewModels.Add(new HistogramBinsViewModel(histogram));
                            break;
                    }
                });
            var timeStepIndex = 0;
            foreach (var timeStepRecord in SimulationLog.Where(timeStepRecord => !SelectedTimeRange || (timeStepRecord.StartTime >= _filterStartTime && _filterEndTime >= timeStepRecord.StartTime))) 
            {
                timeStepRecord.ReadAll();
                timeStepIndex++;
                var record = timeStepRecord;
                if (processTask != null) await processTask;

                processTask = modeThresholdHistogram.Process(record, _dispatcher);

                if (timeStepIndex % 10 == 0) _dispatcher.InvokeIfRequired(UpdateHistogramDisplay);
            }
            if (processTask != null) await processTask;
            _dispatcher.InvokeIfRequired(UpdateHistogramDisplay);
            Debug.WriteLine("Finished processing simulation exposure file");
        }

        public event EventHandler<EventArgs> GraphicsUpdate;
        [Initialize, UsedImplicitly] public Dictionary<Guid, Color> GuidToColorMap { get; private set; }
        [Initialize, UsedImplicitly] public ObservableCollection<PlatformContentFilterRecord> AvailablePlatforms { get; private set; }
        [Initialize, UsedImplicitly] public ObservableCollection<ContentFilterRecordBase> AvailableModes { get; private set; }
        [Initialize, UsedImplicitly] public ObservableCollection<ContentFilterRecordBase> AvailableSpecies { get; private set; }
        readonly List<PropertyObserver<ContentFilterRecordBase>> _platformSelectionObservers = new List<PropertyObserver<ContentFilterRecordBase>>();
        readonly List<PropertyObserver<ContentFilterRecordBase>> _modeSelectionObservers = new List<PropertyObserver<ContentFilterRecordBase>>();
        readonly List<PropertyObserver<ContentFilterRecordBase>> _speciesSelectionObservers = new List<PropertyObserver<ContentFilterRecordBase>>();

        readonly List<Color> _barColors = new List<Color>
        {
            Colors.Red, Colors.Green, Colors.Blue, Colors.Cyan, Colors.Magenta, Colors.DarkKhaki, Colors.Orange, Colors.Purple, Colors.Fuchsia, Colors.Teal, Colors.Black
        };
    }

    public class ContentFilterRecordBase : ViewModelBase
    {
        [UsedImplicitly] readonly PropertyObserver<ContentFilterRecordBase> _propertyObserver;
        public ContentFilterRecordBase(NameGuidRecord nameGuidRecord)
        {
            NameGuidRecord = nameGuidRecord;
            Name = nameGuidRecord.Name;
            IsSelected = true;
            IsEnabled = true;
            _propertyObserver = new PropertyObserver<ContentFilterRecordBase>(this)
                .RegisterHandler(p => p.IsEnabled, () => OnPropertyChanged("IsSelected"));
        }

        bool _isSelected;
        public bool IsSelected { get { return IsEnabled && _isSelected; } set { _isSelected = value; } }
        public bool IsEnabled { get; set; }
        public string Name { get; set; }
        public NameGuidRecord NameGuidRecord { get; private set; }
    }

    public class PlatformContentFilterRecord : ContentFilterRecordBase
    {
        [UsedImplicitly] readonly PropertyObserver<PlatformContentFilterRecord> _propertyObserver;
        public PlatformContentFilterRecord(NameGuidRecord platformRecord, IEnumerable<ContentFilterRecordBase> dependentModes) : base(platformRecord)
        {
            DependentModes = dependentModes.ToList();
            _propertyObserver = new PropertyObserver<PlatformContentFilterRecord>(this)
                .RegisterHandler(p => p.IsSelected, () => { foreach (var mode in DependentModes) mode.IsEnabled = IsSelected; });
        }
        public List<ContentFilterRecordBase> DependentModes { get; set; }
    }
}
