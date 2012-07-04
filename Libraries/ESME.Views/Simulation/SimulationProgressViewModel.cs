using System.ComponentModel;
using System.Globalization;
using ESME.Simulator;
using HRC.Aspects;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Views.Simulation
{
    public class SimulationProgressViewModel : ViewModelBase
    {
        Simulator.Simulation _simulation;

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
                        case "SimulationLog":
                            ((INotifyPropertyChanged)_simulation.SimulationLog.PercentProgress).PropertyChanged += (s1, e1) =>
                            {
                                if (e1.PropertyName != "PercentComplete") return;
                                var percentComplete = ((PercentProgress<SimulationLog>)s1).PercentComplete;
                                LogfileProgressText = percentComplete < 100 ? percentComplete.ToString(CultureInfo.InvariantCulture) + "%" : "Complete";
                            };
                            break;
                    }
                };
            }
        }

        [Initialize("Starting")] public string SimulationProgressText { get; set; }
        public string LogfileProgressText { get; set; }

        #region CancelCommand
        public SimpleCommand<object, EventToCommandArgs> CancelCommand { get { return _cancel ?? (_cancel = new SimpleCommand<object, EventToCommandArgs>(CancelHandler)); } }
        SimpleCommand<object, EventToCommandArgs> _cancel;

        void CancelHandler(EventToCommandArgs args)
        {
            //var parameter = args.CommandParameter;
            Simulation.Cancel();
        }
        #endregion
    }
}
