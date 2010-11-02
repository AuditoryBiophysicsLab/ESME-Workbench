using System;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.IO;
using System.Windows;
using System.Windows.Threading;
using Cinch;
using ESME.NEMO;
using ESMEWorkBench.Data;
using ESMEWorkBench.ViewModels.TransmissionLoss;
using HRC.Navigation;

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
                //_secondsPerTimeStep.DataValue = 10;
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
            get
            {
                return _okCommand ?? (_okCommand = new SimpleCommand<object, object>(
                    delegate
                    {
                        return (SecondsPerTimeStep.DataValue > 0) && (OutputFileName != null) && (!IsRunning);
                    }, 
                    delegate
                    {
                        Run();
                    }));
            }
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
                bw.RunWorkerCompleted += delegate { Complete(); };
                bw.RunWorkerAsync(_experiment);
            }

            //CloseActivePopUpCommand.Execute(true);
        }
        SimpleCommand<object, object> _okCommand;

        void Complete()
        {
            _experiment.AnimatInterface.WriteSpeciesLevelBins(OutputFileName);
            CloseActivePopUpCommand.Execute(true);
        }

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
            var timeStep = new TimeSpan(0, 0, 0, SecondsPerTimeStep.DataValue);
            var scenarioEndTime = scenario.StartTime + scenario.Duration;
            var modeCount = scenario.ModeCount;
            NemoBase.SimulationStepTime = new TimeSpan(0, 0, SecondsPerTimeStep.DataValue);
            foreach (var platform in platforms)
                platform.CalculateBehavior();
 
            // Loop through all time, one timestep per iteration
            for (var curTime = scenario.StartTime; curTime < scenarioEndTime; curTime += timeStep)
            {
                // For the current time step, loop through all platform
                foreach (var platform in platforms)
                {
                    var platformState = platform.BehaviorModel.PlatformStates[curTime];
                    var platformLocation = platformState.Location;
                    var platformCourse = platformState.Course;
                    // For the current platform, loop through all sources
                    foreach (var source in platform.Sources)
                    {
                        // for the current source, loop through all modes
                        foreach (var mode in source.Modes)
                        {
                            var activeTime = mode.ActiveTimeSteps[curTime];
                            // If the current mode is active at least once during the current time step
                            if (activeTime != null)
                            {
                                // Find a TL field that matches the current mode
                                var transmissionLossField = experiment.NearestMatchingTransmissionLoss(mode, platformLocation);
                                if (transmissionLossField == null) throw new ApplicationException(string.Format("No transmission loss fields found for mode {0}", mode.Name));
                                transmissionLossField.LoadData();

                                var horizontalBeamLookDirection = new Course(platformCourse + mode.RelativeBeamAngle);
                                var horizontalBeamLimits = new PieSlice(horizontalBeamLookDirection - (mode.HorizontalBeamWidth / 2), horizontalBeamLookDirection + (mode.HorizontalBeamWidth / 2));
                                var beamRadius = mode.Radius;
                                // Loop through each animat and expose it if necessary
                                foreach (var animat in animats)
                                {
                                    var animatRange = (float)(platformLocation.GetDistanceTo_Meters(animat.Location));
                                    var animatBearing = (float)(platformLocation.GetBearingTo_Degrees(animat.Location));
                                    // If the animat is not within the radius, skip it.
                                    if ((animatRange <= beamRadius) && (horizontalBeamLimits.Contains(animatBearing)))
                                    {
                                        var transmissionLoss = transmissionLossField.Lookup(animatBearing, animatRange, (float) animat.Location.Elevation_meters);
                                        var soundPressureLevel = mode.SourceLevel - transmissionLoss;
                                        if (soundPressureLevel >= 120)
                                        {
                                            animat.CreateLevelBins(modeCount, 120, 6, 15);
                                            animat.RecordExposure(mode.ModeID, soundPressureLevel);
                                        }
                                    }
                                    //if (platform.BehaviorModel.PlatformStates[curTime].ActiveSourceStates
                                }
                                transmissionLossField.DiscardData();
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