using System;
using System.ComponentModel;
using System.Globalization;
using System.Windows;
using System.Windows.Threading;
using HRC.Aspects;
using HRC.Plotting;
using HRC.Utility;
using HRC.Validation;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Simulation
{
    public class SimulationProgressViewModel : ValidatingViewModel
    {
        Simulator.Simulation _simulation;
        const string TimeSpanFormatString = @"hh\:mm\:ss";
        public SimulationProgressViewModel()
        {
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

        [Affects("TimeStepString")]
        public Simulator.Simulation Simulation
        {
            get { return _simulation; }
            set
            {
                _simulation = value;
                if (_simulation == null) return;
                TimeStepString = _simulation.TimeStepSize.ToString(TimeSpanFormatString);
                ((INotifyPropertyChanged)_simulation).PropertyChanged += (s, e) =>
                {
                    switch (e.PropertyName)
                    {
                        case "PercentProgress":
                            ((INotifyPropertyChanged)_simulation.PercentProgress).PropertyChanged += (s1, e1) =>
                            {
                                if (e1.PropertyName != "PercentComplete") return;
                                var percentComplete = ((PercentProgress<Simulator.Simulation>)s1).PercentComplete;
                                SimulationProgressText = percentComplete < 100 ? percentComplete.ToString(CultureInfo.InvariantCulture) + "%" : "Complete";
                            };
                            break;
                    }
                };
            }
        }

        public string TimeStepString { get; set; }

        [Initialize("Starting...")]
        public string SimulationProgressText { get; set; }

        public bool SimulationNotRunning { get { return !IsSimulationRunning; } }

        [Affects("SimulationNotRunning")]
        public bool IsSimulationRunning { get; set; }

        public bool IsSimulationCanceled { get; set; }

        public FourAxisSeriesViewModel AxisSeriesViewModel { get; set; }

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
            task.ContinueWith(t => Window.Dispatcher.InvokeIfRequired(Window.Close));
            IsStartCommandEnabled = false;
            IsSimulationRunning = true;
            OnSimulationStarting();
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
                AxisSeriesViewModel = new FourAxisSeriesViewModel()
                {
                    BottomAxis =
                        {
                            Visibility = Visibility.Visible,
                            Label = "Sound Pressure Level (dB re: 1 µPa)",
                        },
                    LeftAxis =
                        {
                            Visibility = Visibility.Visible,
                            Label = "Exposure count",
                        },
                    TopAxis = { Visibility = Visibility.Collapsed },
                    RightAxis = { Visibility = Visibility.Collapsed },
                    XAxisTicks = null,
                    YAxisTicks = null,
                },
            };
            DesignTimeData.AxisSeriesViewModel.BottomAxis.DataRange = axisRanges;
            DesignTimeData.AxisSeriesViewModel.LeftAxis.DataRange = axisRanges;
        }

    }
}
