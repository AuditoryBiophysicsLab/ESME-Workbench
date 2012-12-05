using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Threading;
using ESME.SimulationAnalysis;
using ESME.Simulator;
using HRC;
using HRC.Aspects;
using HRC.Plotting;
using HRC.Services;
using HRC.Utility;
using HRC.Validation;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Simulation
{
    public class SimulationProgressViewModel : ValidatingViewModel
    {
        readonly IUIVisualizerService _visualizer;
        readonly IMessageBoxService _messageBox;
        Simulator.Simulation _simulation;
        const string TimeSpanFormatString = @"hh\:mm\:ss";

        public SimulationProgressViewModel() {}
        public SimulationProgressViewModel(IUIVisualizerService visualizer, IMessageBoxService messageBox)
        {
            _visualizer = visualizer;
            _messageBox = messageBox;
            AddValidationRules(
                new ValidationRule<SimulationProgressViewModel>
                {
                    PropertyName = "TimeStepString",
                    Description = "Must be a valid, non-negative time span value in the format hh:mm:ss where 00 <= hh <= 23; 00 <= mm <= 59; 00 <= ss <= 59",
                    IsRuleValid = (target, rule) =>
                    {
                        if (string.IsNullOrEmpty(target.TimeStepString)) return false;
                        TimeSpan timeSpan;
                        var isOK = TimeSpan.TryParseExact(target.TimeStepString, TimeSpanFormatString, null, out timeSpan);
                        return isOK && timeSpan.Ticks > 0;
                    },
                });
        }

        public Window Window { get; set; }

        CollectionObserver _modeBinsCollectionObserver;
        [Affects("TimeStepString")]
        public Simulator.Simulation Simulation
        {
            get { return _simulation; }
            set
            {
                _simulation = value;
                if (_simulation == null) return;
                TimeStepString = _simulation.TimeStepSize.ToString(TimeSpanFormatString);
                ((INotifyPropertyChanged)_simulation).PropertyChanged += (sender, args) =>
                {
                    switch (args.PropertyName)
                    {
                        case "PercentProgress":
                            ((INotifyPropertyChanged)_simulation.PercentProgress).PropertyChanged += (s, e) =>
                            {
                                if (e.PropertyName != "PercentComplete") return;
                                var percentComplete = ((PercentProgress<Simulator.Simulation>)s).PercentComplete;
                                SimulationProgressText = percentComplete < 100 ? percentComplete.ToString(CultureInfo.InvariantCulture) + "%" : "Complete";
                            };
                            break;
                        case "ModeThresholdHistogram":
                            _modeBinsCollectionObserver = new CollectionObserver(_simulation.ModeThresholdHistogram.GroupedExposures.Groups)
                                .RegisterHandler((s, e) =>
                                {
                                    switch (e.Action)
                                    {
                                        case NotifyCollectionChangedAction.Add:
                                            foreach (GroupedExposuresHistogram histogram in e.NewItems) 
                                                HistogramBinsViewModels.Add(new HistogramBinsViewModel(histogram));
                                            break;
                                    }
                                });
                            break;
                    }
                };
            }
        }

        public string TimeStepString { get; set; }

        [Initialize("Starting...")]
        public string SimulationProgressText { get; set; }

        public bool SimulationNotRunning { get { return !IsSimulationRunning; } }
        [Initialize(true)] public bool DisplayExposureHistograms { get; set; }

        [Affects("SimulationNotRunning")]
        public bool IsSimulationRunning { get; set; }

        public bool IsSimulationCanceled { get; set; }

        [Initialize, UsedImplicitly] public ObservableCollection<HistogramBinsViewModel> HistogramBinsViewModels { get; private set; }

        #region CancelCommand
        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(CancelHandler)); }
        }

        SimpleCommand<object, object> _cancel;

        void CancelHandler(object o)
        {
            IsSimulationCanceled = true;
            if (!SimulationNotRunning) Simulation.Cancel();
            else Window.Close();
        }
        #endregion

        #region StartCommand
        public SimpleCommand<object, object> StartCommand
        {
            get { return _start ?? (_start = new SimpleCommand<object, object>(o => IsStartCommandEnabled, StartHandler)); }
        }

        SimpleCommand<object, object> _start;

        [Initialize(true)]
        public bool IsStartCommandEnabled { get; set; }

        void StartHandler(object o)
        {
            var task = Simulation.Start(TimeSpan.ParseExact(TimeStepString, TimeSpanFormatString, null));

            task.ContinueWith(t =>
            {
                if (t.IsFaulted) _messageBox.ShowError(string.Format("The simulation encountered an error: {0}", t.Exception.InnerExceptions[0].Message));
                else Window.Dispatcher.InvokeIfRequired(Window.Close);
            },
                              TaskScheduler.FromCurrentSynchronizationContext());
            IsStartCommandEnabled = false;
            IsSimulationRunning = true;
            OnSimulationStarting();
            if (DisplayExposureHistograms) _visualizer.ShowWindow("SimulationExposuresView", new SimulationExposuresViewModel(HistogramBinsViewModels));
            //task.Wait();
        }

        public event EventHandler SimulationStarting;

        protected void OnSimulationStarting()
        {
            var handlers = SimulationStarting;
            if (handlers == null) return;
            foreach (EventHandler handler in handlers.GetInvocationList())
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
        #endregion

        public static SimulationProgressViewModel DesignTimeData { get; set; }
        static SimulationProgressViewModel()
        {
            var axisRanges = new RangeCollection();
            axisRanges.Add(new Range(0.1, 10));
            DesignTimeData = new SimulationProgressViewModel
            {
                HistogramBinsViewModels =
                {
                    HistogramBinsViewModel.DesignTimeData
                },
            };
        }

    }
}
