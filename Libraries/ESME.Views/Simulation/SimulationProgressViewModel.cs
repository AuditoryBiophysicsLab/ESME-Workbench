using System;
using System.ComponentModel;
using System.Globalization;
using System.Windows;
using System.Windows.Threading;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Simulation
{
    public class SimulationProgressViewModel : ViewModelBase
    {
        Simulator.Simulation _simulation;

        public Window Window { get; set; }

        [Affects("TimeStepString")]
        public Simulator.Simulation Simulation
        {
            get { return _simulation; }
            set
            {
                _simulation = value;
                if (_simulation == null) return;
                _timeStepSize = _simulation.TimeStepSize;
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

        TimeSpan _timeStepSize;
        public string TimeStepString
        {
            get { return _timeStepSize.ToString(@"hh\:mm\:ss"); }
            set
            {
                IsStartCommandEnabled = TimeSpan.TryParseExact(value, @"hh\:mm\:ss", null, out _timeStepSize);
            }
        }

        [Initialize("Starting...")]
        public string SimulationProgressText { get; set; }

        public bool SimulationNotRunning { get { return !IsSimulationRunning; } }

        [Affects("SimulationNotRunning")]
        public bool IsSimulationRunning { get; set; }

        public bool IsSimulationCanceled { get; set; }

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
            var task = Simulation.Start(_timeStepSize);
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
    }
}
