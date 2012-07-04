using System.ComponentModel;
using System.Globalization;
using ESME.Simulator;
using HRC.Utility;
using HRC.ViewModels;

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

        public string SimulationProgressText { get; set; }
        public string LogfileProgressText { get; set; }
    }
}
