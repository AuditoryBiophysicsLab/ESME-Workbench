using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using Cinch;
using ESMEWorkBench.Data;
using ESMEWorkBench.ViewModels.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.Main
{
    internal class SimulationViewModel : EditableValidatingViewModelBase
    {
        #region public constructor

        readonly Experiment _experiment;

        public SimulationViewModel(Experiment experiment)
        {
            _experiment = experiment;

            SecondsPerTimeStep = new LabeledDataWrapper<int>(this, SecondsPerTimeStepChangedEventArgs)
                                 {
                                     IsEditable = true,
                                 };
            Parameters = new ObservableCollection<LabelValuePair>
                         {
                             new LabelValuePair
                             {
                                 Label = "Experiment name",
                                 Value = _experiment.FileName,
                             },
                             new LabelValuePair
                             {
                                 Label = "Experiment type",
                                 Value = "Static animals",
                             },
                             new LabelValuePair
                             {
                                 Label = "Scenario file name",
                                 Value = Path.GetFileName(_experiment.ScenarioFileName),
                             },
                             new LabelValuePair
                             {
                                 Label = "Scenario Description",
                                 Value = _experiment.NemoFile.Scenario.Description,
                             },
                             new LabelValuePair
                             {
                                 Label = "Scenario duration",
                                 Value = _experiment.NemoFile.Scenario.Duration.ToString(),
                             },
                             new LabelValuePair
                             {
                                 Label = "Number of platforms",
                                 Value = _experiment.NemoFile.Scenario.Platforms.Count.ToString(),
                             },
                             new LabelValuePair
                             {
                                 Label = "Total animal count",
                                 Value = "TBD - add to Experiment please!",
                             },
                             new LabelValuePair
                             {
                                 Label = "Species count",
                                 Value = "TBD - add to Experiment please!",
                             },
                             new LabelValuePair
                             {
                                 Label = "Estimated run time",
                                 Value = "Oh, a fair while",
                             },
                         };
        }

        #endregion

        #region public LabeledDataWrapper<int> SecondsPerTimeStep { get; set; }

        public LabeledDataWrapper<int> SecondsPerTimeStep
        {
            get { return _secondsPerTimeStep; }
            set
            {
                if (_secondsPerTimeStep == value) return;
                _secondsPerTimeStep = value;
                _secondsPerTimeStep.ValidationRules.Add(new SimpleRule("DataValue", "Value must be an integer between 1 and 100, inclusive", domObj =>
                                                                                                                                             {
                                                                                                                                                 var obj = (DataWrapper<int>) domObj;
                                                                                                                                                 return ((obj.DataValue < 1) || (100 < obj.DataValue));
                                                                                                                                             }));
                NotifyPropertyChanged(SecondsPerTimeStepChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs SecondsPerTimeStepChangedEventArgs = ObservableHelper.CreateArgs<SimulationViewModel>(x => x.SecondsPerTimeStep);
        LabeledDataWrapper<int> _secondsPerTimeStep;

        #endregion

        #region public ObservableCollection<LabelValuePair> Parameters { get; set; }

        public ObservableCollection<LabelValuePair> Parameters
        {
            get { return _parameters; }
            set
            {
                if (_parameters == value) return;
                if (_parameters != null) _parameters.CollectionChanged -= ParametersCollectionChanged;
                _parameters = value;
                if (_parameters != null) _parameters.CollectionChanged += ParametersCollectionChanged;
                NotifyPropertyChanged(ParametersChangedEventArgs);
            }
        }

        void ParametersCollectionChanged(object sender, NotifyCollectionChangedEventArgs e) { NotifyPropertyChanged(ParametersChangedEventArgs); }
        static readonly PropertyChangedEventArgs ParametersChangedEventArgs = ObservableHelper.CreateArgs<SimulationViewModel>(x => x.Parameters);
        ObservableCollection<LabelValuePair> _parameters;

        #endregion
    }

    public class LabelValuePair
    {
        public string Label { get; set; }
        public string Value { get; set; }
    }
}