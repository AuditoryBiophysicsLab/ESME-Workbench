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

        public Simulator.Simulation Simulation
        {
            get { return _simulation; }
            set
            {
                _simulation = value;
                if (_simulation == null) return;
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

        [Initialize("Press start to begin")]
        public string SimulationProgressText { get; set; }

        #region CancelCommand
        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(o => IsCancelCommandEnabled, CancelHandler)); }
        }

        SimpleCommand<object, object> _cancel;

        bool IsCancelCommandEnabled { get; set; }

        void CancelHandler(object o) { Simulation.Cancel(); }
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
            var task = Simulation.Start();
            task.ContinueWith(t => Window.Dispatcher.InvokeIfRequired(Window.Close));
            IsStartCommandEnabled = false;
            IsCancelCommandEnabled = true;
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
