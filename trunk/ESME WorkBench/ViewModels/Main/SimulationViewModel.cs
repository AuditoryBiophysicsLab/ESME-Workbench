using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESMEWorkBench.Data;
using ESMEWorkBench.ViewModels.TransmissionLoss;

namespace ESMEWorkBench.ViewModels.Main
{
    internal class SimulationViewModel : EditableValidatingViewModelBase, IViewStatusAwareInjectionAware
    {
        IViewAwareStatus _viewAwareStatus;
        Dispatcher _dispatcher;

        #region public constructor

        readonly Experiment _experiment;

        public SimulationViewModel(Experiment experiment)
        {
            _experiment = experiment;
            IsRunning = false;

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
                                 Value = _experiment.AnimatInterface.AnimatList.Count.ToString(),
                             },
                             new LabelValuePair
                             {
                                 Label = "Species count",
                                 Value = _experiment.AnimatInterface.AnimatList.SpeciesList.Count.ToString(),
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

        #region public string OutputFileName { get; set; }

        public string OutputFileName
        {
            get { return _outputFileName; }
            set
            {
                if (_outputFileName == value) return;
                _outputFileName = value;
                NotifyPropertyChanged(OutputFileNameChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs OutputFileNameChangedEventArgs = ObservableHelper.CreateArgs<SimulationViewModel>(x => x.OutputFileName);
        string _outputFileName;

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

        public bool IsRunning { get; set; }

        public bool IsCompleted { get; set; }

        #region CancelCommand

        public SimpleCommand<object, object> CancelCommand
        {
            get { return _cancel ?? (_cancel = new SimpleCommand<object, object>(x => Cancel())); }
        }

        void Cancel()
        {
            if (!IsRunning) CloseActivePopUpCommand.Execute(false);
        }

        SimpleCommand<object, object> _cancel;

        #endregion

        #region OkCommand

        public SimpleCommand<object, object> OkCommand
        {
            get { return _okCommand ?? (_okCommand = new SimpleCommand<object, object>(x=> (SecondsPerTimeStep.DataValue > 0) && (OutputFileName != null) && (!IsRunning), x => Run())); }
        }

        void Run()
        {
            lock (this)
            {
                if (IsRunning) return;
                IsRunning = true;
                var bw = new BackgroundWorker();
                bw.DoWork += CalculateExposures;
                bw.RunWorkerCompleted += delegate { IsCompleted = true; };
                bw.RunWorkerAsync(_experiment);
            }

            CloseActivePopUpCommand.Execute(true);
        }
        SimpleCommand<object, object> _okCommand;

        #endregion

        #region IViewStatusAwareInjectionAware Members

        public void InitialiseViewAwareService(IViewAwareStatus viewAwareStatusService)
        {
            _viewAwareStatus = viewAwareStatusService;
            _dispatcher = ((Window)_viewAwareStatus.View).Dispatcher;
        }

        #endregion


        void CalculateExposures(object sender, DoWorkEventArgs args)
        {
            var experiment = (Experiment)args.Argument;
            var animats = experiment.AnimatInterface.AnimatList;
            var scenario = experiment.NemoFile.Scenario;
            var platforms = scenario.Platforms;
            var timeStepCount = (int)(scenario.Duration.TotalSeconds / SecondsPerTimeStep.DataValue);
            var timeStep = new TimeSpan(0, 0, 0, SecondsPerTimeStep.DataValue);
            var scenarioEndTime = scenario.StartTime + scenario.Duration;
            for (var curTime = scenario.StartTime; curTime <= scenarioEndTime; curTime += timeStep)
            {
                foreach (var platform in platforms)
                {
                    foreach (var source in platform.Sources)
                    {
                        foreach (var mode in source.Modes)
                        {
                            // Find a TL field that matches the current mode
                            // Loop through each animat and expose it if necessary
                            foreach (var animat in animats)
                            {
                                // If the animat is not within the radius, skip it.
                                //if (platform.BehaviorModel.PlatformStates[curTime].ActiveSourceStates
                            }
                        }
                    }
                }
            }

            //_dispatcher.InvokeIfRequired(() => NotifyPropertyChanged());
        }

    }

    public class LabelValuePair
    {
        public string Label { get; set; }
        public string Value { get; set; }
    }
}