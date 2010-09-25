using System;
using System.ComponentModel;
using System.ComponentModel.Composition;
using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Input;
using Cinch;
using ESME.Environment;
using ESME.Model;
using ESME.TransmissionLoss;
using ESME.TransmissionLoss.Bellhop;
using ESMEWorkBench.Data;
using ESMEWorkBench.Properties;
using ESMEWorkBench.ViewModels.TransmissionLoss;
using HRC.Navigation;
using MEFedMVVM.ViewModelLocator;
using BathymetryOutOfBoundsException = ESME.Model.BathymetryOutOfBoundsException;

namespace ESMEWorkBench.ViewModels.Main
{
    [ExportViewModel("ExperimentViewModel")]
    public partial class ExperimentViewModel : ViewModelBase
    {
        #region Private fields

        readonly IMessageBoxService _messageBoxService;
        readonly IOpenFileService _openFileService;
        readonly ISaveFileService _saveFileService;
        readonly IViewAwareStatus _viewAwareStatusService;
        readonly IUIVisualizerService _visualizerService;
        Experiment _experiment;
        string _environmentFileName;
        #endregion

        #region Constructors

        [ImportingConstructor]
        public ExperimentViewModel(IViewAwareStatus viewAwareStatusService, IMessageBoxService messageBoxService, IOpenFileService openFileService, ISaveFileService saveFileService, IUIVisualizerService visualizerService)
        {
            try
            {
                Mediator.Instance.Register(this);
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.WriteLine("***********\nExperimentViewModel: Mediator registration failed: " + ex.Message + "\n***********");
                throw;
            }
            _viewAwareStatusService = viewAwareStatusService;
            _messageBoxService = messageBoxService;
            _openFileService = openFileService;
            _saveFileService = saveFileService;
            _visualizerService = visualizerService;

            _experiment = new Experiment();
        }

        protected override void OnDispose()
        {
            base.OnDispose();
            Mediator.Instance.Unregister(this);
        }

        #endregion

        #region ViewModel properties

        #region public string DecoratedExperimentName { get; set; }

        static readonly PropertyChangedEventArgs DecoratedExperimentNameChangedEventArgs = ObservableHelper.CreateArgs<ExperimentViewModel>(x => x.DecoratedExperimentName);
        string _decoratedExperimentName;

        public string DecoratedExperimentName
        {
            get { return _decoratedExperimentName; }
            set
            {
                if (_decoratedExperimentName == value) return;
                _decoratedExperimentName = value;
                NotifyPropertyChanged(DecoratedExperimentNameChangedEventArgs);
            }
        }

        #endregion

        #region public EarthCoordinate MouseEarthCoordinate { get; set; }

        public EarthCoordinate MouseEarthCoordinate
        {
            get { return _mouseEarthCoordinate; }
            set
            {
                if (_mouseEarthCoordinate == value) return;
                _mouseEarthCoordinate = value;
                NotifyPropertyChanged(MouseEarthCoordinateChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs MouseEarthCoordinateChangedEventArgs = ObservableHelper.CreateArgs<ExperimentViewModel>(x => x.MouseEarthCoordinate);
        EarthCoordinate _mouseEarthCoordinate;

        #endregion

        #endregion

        #region Drag/Drop

        public bool CanAddScenarioFile() { return ((Globals.AppSettings.ScenarioDataDirectory != null) && (Directory.Exists(Globals.AppSettings.ScenarioDataDirectory))); }

        public void FilesDropped(Object sender, DragEventArgs e)
        {
            if (!e.Data.GetDataPresent(DataFormats.FileDrop)) return;
            var droppedFilePaths = (string[]) e.Data.GetData(DataFormats.FileDrop, true);
            foreach (var file in droppedFilePaths)
            {
                try
                {
                    switch (Path.GetExtension(file).ToLower())
                    {
                        case ".shp":
                            Mediator.Instance.NotifyColleagues("AddFileLayerToMapViewMessage", file);
                            break;
                        case ".ovr":
                            Mediator.Instance.NotifyColleagues("AddFileLayerToMapViewMessage", file);
                            break;
                        case ".eeb":
                            _environmentFileName = file;
                            Mediator.Instance.NotifyColleagues("AddFileLayerToMapViewMessage", file);
                            break;
                        case ".nemo":
                            if (CanAddScenarioFile()) Mediator.Instance.NotifyColleagues("AddFileLayerToMapViewMessage", file);
                            break;
                    }
                }
                catch (Exception ex)
                {
                    Globals.DisplayException(_messageBoxService, ex, "Error opening dropped file {0}", file);
                }
            }
            Mediator.Instance.NotifyColleagues("RefreshMapViewMessage");
        }

        #endregion

        #region Experiment Load/Save and associated utility functions

        /// <summary>
        ///   If the current experiment is unsaved, ask the user to save the experiment.
        /// </summary>
        /// <returns>true if the user wants to cancel the current operation, false otherwise.</returns>
        bool UserCanceledBecauseExperimentUnsaved()
        {
            if ((_experiment == null) || (!_experiment.IsChanged)) return false;
            var results = _messageBoxService.ShowYesNoCancel("The current experiment has changed.\nWould you like to save it first?", CustomDialogIcons.Exclamation);
            if (results == CustomDialogResults.Cancel) return true;
            if (results == CustomDialogResults.No) return false;

            return !SaveExperiment();
        }

        void OpenExperiment(string fileName)
        {
            if (UserCanceledBecauseExperimentUnsaved()) return;
            if (fileName == null)
            {
                _openFileService.Filter = "ESME files (*.esme)|*.esme|All files (*.*)|*.*";
                _openFileService.InitialDirectory = Settings.Default.LastExperimentFileDirectory;
                var result = _openFileService.ShowDialog((Window) _viewAwareStatusService.View);
                if ((!result.HasValue) || (!result.Value)) return;
                _experiment.FileName = _openFileService.FileName;
                Settings.Default.LastExperimentFileDirectory = Path.GetDirectoryName(_openFileService.FileName);
            }
            _experiment = Experiment.Load(fileName);
        }

        void LoadExperiment()
        {
            var args = Environment.GetCommandLineArgs();
            if (args.Length == 2)
            {
                if (File.Exists(args[1]))
                {
                    if (args[1].EndsWith(".esme"))
                    {
                        try
                        {
                            _experiment = Experiment.Load(args[1]);
                            Globals.IsInitializeExperimentNeeded = true;
                            DecoratedExperimentName = Path.GetFileName(args[1]);
                        }
                        catch (Exception ex)
                        {
                            _messageBoxService.ShowError(string.Format("Error opening experiment file \"{0}\":\n{1}", args[1], ex.Message));
                        }
                    }
                }
            }
            else
            {
                _experiment = new Experiment();
                DecoratedExperimentName = "<New experiment>";
            }
            _experiment.PropertyChanged += delegate(object s, PropertyChangedEventArgs e)
                                           {
                                               switch (e.PropertyName)
                                               {
                                                   case "IsChanged":
                                                       if (_experiment.IsChanged)
                                                       {
                                                           if (DecoratedExperimentName.EndsWith(" *")) return;
                                                           DecoratedExperimentName += " *";
                                                       }
                                                       else
                                                       {
                                                           if (!DecoratedExperimentName.EndsWith(" *")) return;
                                                           DecoratedExperimentName.Remove(DecoratedExperimentName.Length - 2);
                                                       }
                                                       break;
                                               }
                                           };

            //if ((Globals.Experiment.ScenarioFileName != null) && (File.Exists(Globals.Experiment.ScenarioFileName)))
            //    AddScenarioFile(Globals.Experiment.ScenarioFileName);
            //else Globals.Experiment.ScenarioFileName = null;
        }

        /// <summary>
        ///   If the experiment has not been given a file name, prompt the user for it, then save
        /// </summary>
        /// <returns>true if the file was saved, false otherwise.</returns>
        bool SaveExperiment()
        {
            if ((_experiment == null) || (!_experiment.IsChanged)) return true;
            if (_experiment.FileName == null)
            {
                _saveFileService.Filter = "ESME files (*.esme)|*.esme|All files (*.*)|*.*";
                _saveFileService.OverwritePrompt = true;
                _saveFileService.InitialDirectory = Settings.Default.LastExperimentFileDirectory;
                var result = _saveFileService.ShowDialog((Window) _viewAwareStatusService.View);
                if ((!result.HasValue) || (!result.Value)) return false;
                _experiment.FileName = _saveFileService.FileName;
                Settings.Default.LastExperimentFileDirectory = Path.GetDirectoryName(_saveFileService.FileName);
            }
            _experiment.Save();
            return true;
        }

        #endregion

        bool UserWantsToReplaceScenarioFileIfPresent(string filename)
        {
            if ((_experiment != null) && (_experiment.ScenarioFileName != null) && (filename != _experiment.ScenarioFileName))
            {
                var result = _messageBoxService.ShowYesNo(string.Format("This experiment already has a scenario file.  Replace it with \"{0}\"", filename), CustomDialogIcons.Exclamation);
                if (result == CustomDialogResults.No) return false;
            }
            return true;
        }

        [MediatorMessageSink("SetMouseEarthCoordinateMessage")]
        void SetMouseEarthCoordinate(EarthCoordinate mouseEarthCoordinate) { MouseEarthCoordinate = mouseEarthCoordinate; }

        [MediatorMessageSink("RunQuickLookMessage")]
        void RunQuickLook(bool dummy) 
        {
            #region create transmission loss job. the new base class for all acoustic simulations!

            var transmissionLossJob = new TransmissionLossJob
            {
                AcousticProperties = new AcousticProperties
                {
                    SourceDepth = 10,
                    VerticalBeamWidth = 120,
                    DepressionElevationAngle = 0f,
                    LowFrequency = 3500,
                    HighFrequency = 3500,
                },
                AnalysisPoint = new AnalysisPoint
                {
                    IDField = 1,
                    Location = MouseEarthCoordinate,
                    RadialBearing = 0,
                    RadialCount = 16,
                },
                Radius = 10000,
                MaxDepth = 3000,
            };

            #endregion

            #region create bellhop run file from tlj (and stuff)

            var environmentInformation = new EnvironmentInformation
            {
                Bathymetry = new Bathymetry(_environmentFileName),
                SoundSpeedField = new SoundSpeedField(_environmentFileName),
                Sediment = SedimentTypes.SedimentArray[0],
            };

            var transmissionLossSettings = new TransmissionLossSettings
            {
                DepthCellSize = 50,
                RangeCellSize = 50,
            };

            var transmissionLossJobViewModel = new TransmissionLossJobViewModel
            {
                TransmissionLossJob = transmissionLossJob
            };
            var result = _visualizerService.ShowDialog("TransmissionLossJobView", transmissionLossJobViewModel);
            if ((!result.HasValue) || (!result.Value))
            {
                Mediator.Instance.NotifyColleagues("SetMapCursorMessage", Cursors.Arrow);
                return;
            }

            Mediator.Instance.NotifyColleagues("SetMapCursorMessage", Cursors.Wait);

            TransmissionLossField transmissionLossField;
            try
            {
                var bellhopRunFile = BellhopRunFile.Create(transmissionLossJob, environmentInformation, transmissionLossSettings);
                transmissionLossField = FieldCalculator.ComputeField(bellhopRunFile, null);
            }
            catch (BathymetryOutOfBoundsException)
            {
                _messageBoxService.ShowError("Unable to run quick look.\nDid you click outside the bounds of the environment file?");
                Mediator.Instance.NotifyColleagues("SetMapCursorMessage", Cursors.Arrow);
                return;
            }
            catch (AggregateException ex)
            {
                var sb = new StringBuilder();
                foreach (var e in ex.InnerExceptions) sb.Append(e.Message + "\n");
                _messageBoxService.ShowError("One or more errors occurred calculating transmission loss:\n" + sb);
                Mediator.Instance.NotifyColleagues("SetMapCursorMessage", Cursors.Arrow);
                return;
            }
            var transmissionLossViewModel = new TransmissionLossFieldViewModel(transmissionLossField);
            transmissionLossField.Filename = @".\test.tlf";
            transmissionLossField.Save();
            _visualizerService.Show("TransmissionLossView", transmissionLossViewModel, true, null);

            Mediator.Instance.NotifyColleagues("SetMapCursorMessage", Cursors.Arrow);

            #endregion

        }
    }
}