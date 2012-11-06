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
                .RegisterHandler(p => p.IsAnalyzeCommandEnabled, CommandManager.InvalidateRequerySuggested)
                .RegisterHandler(p => p.PerformOurOnlyAnalysis, () => IsAnalyzeCommandEnabled = PerformOurOnlyAnalysis)
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
                Description = string.Format("Must be between {0} and {1}", _simulationStartTime.ToString(TimeSpanFormatString), _simulationEndTime.ToString(TimeSpanFormatString)),
                IsRuleValid = (target, rule) =>
                {
                    if (AllTimes) return true;
                    if (_simulationStartTime.Ticks == 0 && _simulationEndTime.Ticks == 0) return true;
                    if (string.IsNullOrEmpty(target.StartTimeString)) return false;
                    TimeSpan timeSpan;
                    var isOK = TimeSpan.TryParseExact(target.StartTimeString, TimeSpanFormatString, null, out timeSpan);
                    return isOK && timeSpan.Ticks > 0;
                },
            };
            _endTimeValidationRule = new ValidationRule<SimulationLogAnalysisMainViewModel>
            {
                PropertyName = "StopTimeString",
                Description = string.Format("Must be between {0} and {1}", _simulationStartTime.ToString(TimeSpanFormatString), _simulationEndTime.ToString(TimeSpanFormatString)),
                IsRuleValid = (target, rule) =>
                {
                    if (string.IsNullOrEmpty(target.StopTimeString)) return false;
                    TimeSpan timeSpan;
                    var isOK = TimeSpan.TryParseExact(target.StopTimeString, TimeSpanFormatString, null, out timeSpan);
                    return isOK && timeSpan.Ticks > 0;
                },
            };
        }

        void StartOrStopTimeStringsChanged() 
        {
            TimeSpan timeSpan;
            var isOK = TimeSpan.TryParseExact(StartTimeString, TimeSpanFormatString, null, out timeSpan);
            if (isOK) _filterStartTime = timeSpan;
            isOK = TimeSpan.TryParseExact(StopTimeString, TimeSpanFormatString, null, out timeSpan);
            if (isOK) _filterStartTime = timeSpan;
        }

        [Initialize, UsedImplicitly] public ObservableCollection<HistogramBinsViewModel> HistogramBinsViewModels { get; private set; }

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
            foreach (var mode in SimulationLog.ModeRecords) AvailableModes.Add(new ContentFilterRecordBase(mode) { Name = string.Format("{0}:{1}", mode.PlatformRecord.Name, mode.Name) });
            foreach (var platform in SimulationLog.PlatformRecords)
            {
                var curPlatform = platform;
                var dependentModes = from mode in AvailableModes
                                     where ((ModeNameGuid)mode.NameGuidRecord).PlatformGuid == curPlatform.Guid
                                     select mode;
                AvailablePlatforms.Add(new PlatformContentFilterRecord(curPlatform, dependentModes));
            }
            foreach (var species in SimulationLog.SpeciesRecords) AvailableSpecies.Add(new ContentFilterRecordBase(species));
            for (var speciesIndex = 0; speciesIndex < SimulationLog.SpeciesRecords.Count; speciesIndex++) GuidToColorMap.Add(SimulationLog.SpeciesRecords[speciesIndex].Guid, _barColors[speciesIndex % _barColors.Count]);
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

        [Initialize(true)] bool IsAnalyzeCommandEnabled { get; set; }
        void AnalyzeHandler(EventToCommandArgs args) { TaskEx.Run(PerformAnalysis); }
        #endregion
        async void PerformAnalysis()
        {
            Task<bool> processTask = null;
            _dispatcher.InvokeIfRequired(() => OpenWindows.Add(_visualizer.ShowWindow("SimulationExposuresView", new SimulationExposuresViewModel(HistogramBinsViewModels))));
            ModeThresholdHistogram = new ModeThresholdHistogram(this, SimulationLog, StartBinValue, BinWidth, BinCount);
            _modeBinsCollectionObserver = new CollectionObserver(ModeThresholdHistogram.GroupedExposures.Groups)
                .RegisterHandler((s, e) =>
                {
                    switch (e.Action)
                    {
                        case NotifyCollectionChangedAction.Add:
                            foreach (GroupedExposuresHistogram histogram in e.NewItems) HistogramBinsViewModels.Add(new HistogramBinsViewModel(histogram));
                            break;
                    }
                });
            var timeStepIndex = 0;
            foreach (var timeStepRecord in SimulationLog)
            {
                if (!AllTimes && _filterStartTime < timeStepRecord.StartTime && timeStepRecord.StartTime > _filterEndTime)
                {
                    Debug.WriteLine(string.Format("Discarding record with StartTime: {0}", timeStepRecord.StartTime));
                    continue;
                }
                Debug.WriteLine(string.Format("Processing record with StartTime: {0}", timeStepRecord.StartTime));
                timeStepRecord.ReadAll();
                timeStepIndex++;
                var record = timeStepRecord;
                if (processTask != null) await processTask;
                processTask = ModeThresholdHistogram.Process(record, _dispatcher);
                if (timeStepIndex % 10 == 0) _dispatcher.InvokeIfRequired(UpdateHistogramDisplay);
            }
            if (processTask != null) await processTask;
            _dispatcher.InvokeIfRequired(UpdateHistogramDisplay);
            Debug.WriteLine("Finished processing simulation exposure file");
        }

        public ModeThresholdHistogram ModeThresholdHistogram { get; set; }

        public event EventHandler<EventArgs> GraphicsUpdate;
        [Initialize, UsedImplicitly] public Dictionary<Guid, Color> GuidToColorMap { get; private set; }
        [Initialize, UsedImplicitly] public ObservableCollection<PlatformContentFilterRecord> AvailablePlatforms { get; private set; }
        [Initialize, UsedImplicitly] public ObservableCollection<ContentFilterRecordBase> AvailableModes { get; private set; }
        [Initialize, UsedImplicitly] public ObservableCollection<ContentFilterRecordBase> AvailableSpecies { get; private set; }

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
