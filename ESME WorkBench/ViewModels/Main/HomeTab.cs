using System;
using System.Linq;
using System.ComponentModel;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;
using Cinch;
using ESME;
using ESME.Mapping;
using ESME.Metadata;
using ESME.Model;
using ESME.NewNEMO;
using ESME.TransmissionLoss;
using ESME.Views.AcousticBuilder;
using ESME.Views.TransmissionLoss;
using ESMEWorkBench.Properties;

namespace ESMEWorkBench.ViewModels.Main
{
    public partial class MainViewModel
    {
        public MapLayerCollection ScenarioMapLayers { get; set; }

        #region public NAEMOScenarioMetadata ScenarioMetadata { get; set; }

        public NAEMOScenarioMetadata ScenarioMetadata
        {
            get { return _scenarioMetadata; }
            set
            {
                if (_scenarioMetadata == value) return;
                if (_scenarioMetadata != null) _scenarioMetadata.Save();
                _scenarioMetadata = value;
                _dispatcher.InvokeIfRequired(() => NotifyPropertyChanged(ScenarioMetadataChangedEventArgs));
                if (_scenarioMetadata != null)
                {
                    _scenarioMetadata.Dispatcher = _dispatcher;
                    _scenarioMetadata.VisualizerService = _visualizerService;
                    _scenarioMetadata.RangeComplexDescriptors = RangeComplexDescriptors;
                    _dispatcher.InvokeIfRequired(() =>
                    {
                        MapLayerCollections.Add("Scenario", Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"Sample GIS Data\Countries02.shp"));
                        ScenarioMapLayers = MapLayerCollections["Scenario"];
                        _scenarioMetadata.MapLayers = ScenarioMapLayers;
                        MapLayerCollections.ActiveLayer = ScenarioMapLayers;
                    });
                }
                else
                {
                    _dispatcher.InvokeIfRequired(() =>
                    {
                        MapLayerCollections.ActiveLayer = MapLayerCollections["Home"];
                        if (ScenarioMapLayers != null) MapLayerCollections.Remove(ScenarioMapLayers);
                    });
                }
                IsScenarioLoaded = _scenarioMetadata != null;
            }
        }

        static readonly PropertyChangedEventArgs ScenarioMetadataChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.ScenarioMetadata);
        NAEMOScenarioMetadata _scenarioMetadata;

        #endregion

        #region public bool IsScenarioLoaded { get; set; }

        public bool IsScenarioLoaded
        {
            get { return _isScenarioLoaded; }
            set
            {
                if (_isScenarioLoaded == value) return;
                _isScenarioLoaded = value;
                NotifyPropertyChanged(IsScenarioLoadedChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsScenarioLoadedChangedEventArgs = ObservableHelper.CreateArgs<MainViewModel>(x => x.IsScenarioLoaded);
        bool _isScenarioLoaded;

        #endregion

        #region OpenScenarioCommand
        public SimpleCommand<object, object> OpenScenarioCommand
        {
            get { return _openScenario ?? (_openScenario = new SimpleCommand<object, object>(delegate { OpenScenarioHandler(null); })); }
        }

        SimpleCommand<object, object> _openScenario;

        void OpenScenarioHandler(string fileName)
        {
            _openFileService.FileName = null;
            if (fileName == null)
            {
                _openFileService.Filter = "NUWC Scenario Files (*.nemo)|*.nemo";
                _openFileService.InitialDirectory = Settings.Default.LastScenarioFileDirectory;
                _openFileService.FileName = null;
                var result = _openFileService.ShowDialog((Window)_viewAwareStatus.View);
                if (!result.HasValue || !result.Value) return;
                fileName = _openFileService.FileName;
                Settings.Default.LastScenarioFileDirectory = Path.GetDirectoryName(_openFileService.FileName);
            }
            ScenarioMetadata = null;
            RecentFiles.InsertFile(fileName);
            NemoScenario.Test(fileName);
            try
            {
                Task.Factory.StartNew(() =>
                {
                    ScenarioMetadata = NAEMOScenarioMetadata.Load(NAEMOMetadataBase.MetadataFilename(fileName)) ?? new NAEMOScenarioMetadata { Filename = NAEMOMetadataBase.MetadataFilename(fileName) };
                    _dispatcher.InvokeIfRequired(() => ScenarioMetadata.ScenarioFilename = fileName);
                });
            }
            catch (Exception ex)
            {
                var sb = new StringBuilder();
                sb.AppendLine(ex.Message);
                var inner = ex.InnerException;
                while (inner != null)
                {
                    sb.AppendLine(inner.Message);
                    inner = inner.InnerException;
                }
                _messageBoxService.ShowError("Error opening scenario \"" + Path.GetFileName(fileName) + "\": " + sb);
                ScenarioMetadata = null;
            }
        }
        #endregion

        #region CloseScenarioCommand
        public SimpleCommand<object, object> CloseScenarioCommand
        {
            get { return _closeScenario ?? (_closeScenario = new SimpleCommand<object, object>(delegate { return IsScenarioLoaded; }, delegate { CloseScenarioHandler(); })); }
        }

        SimpleCommand<object, object> _closeScenario;

        void CloseScenarioHandler()
        {
            ScenarioMetadata = null;
        }
        #endregion

        #region ConfigureAcousticModelsCommand
        public SimpleCommand<object, object> ConfigureAcousticModelsCommand
        {
            get { return _configureAcousticModels ?? (_configureAcousticModels = new SimpleCommand<object, object>(delegate { ConfigureAcousticModelsHandler(); })); }
        }

        SimpleCommand<object, object> _configureAcousticModels;

        void ConfigureAcousticModelsHandler()
        {
            var modeAcousticModelSelectionViewModel = new ModeAcousticModelSelectionViewModel(ScenarioMetadata.NemoModeToAcousticModelNameMap, ESME.Globals.ValidTransmissionLossAlgorithms);
            _visualizerService.ShowDialog("ModeAcousticModelSelectionView", modeAcousticModelSelectionViewModel);
        }

        #endregion

        #region ExportAnalysisPointsCommand
        public SimpleCommand<object, object> ExportAnalysisPointsCommand
        {
            get
            {
                return _exportAnalysisPoints ??
                       (_exportAnalysisPoints =
                        new SimpleCommand<object, object>(delegate { return IsScenarioLoaded; },
                                                          delegate { ExportAnalysisPointsHandler(); }));
            }
        }

        SimpleCommand<object, object> _exportAnalysisPoints;

        void ExportAnalysisPointsHandler() { ScenarioMetadata.ExportAnalysisPoints(); }
        #endregion

        [MediatorMessageSink(MediatorMessage.PlaceAnalysisPoint)]
        void PlaceAnalysisPoint(bool dummy)
        {
            if (MouseDepth > 0) throw new AnalysisPointLocationException("Analysis Points cannot be placed on land.");
            if (ScenarioMetadata == null) return;
            ScenarioMetadata.PlaceAnalysisPoint(MouseEarthCoordinate);
            return;
            try
            {
#if false
            var environmentInformation = new EnvironmentInformation
                                         {
                                             Bathymetry = _experiment.Bathymetry,
                                             SoundSpeedField = _experiment.SoundSpeedField,
                                             Sediment = SedimentTypes.SedimentArray[0],
                                         };

            var transmissionLossSettings = new TransmissionLossSettings
                                           {
                                               DepthCellSize = (float)_experiment.BellhopDepthCellSize,
                                               RangeCellSize = (float)_experiment.BellhopRangeCellSize,
                                           };

#endif
                //if(!_experiment.Bathymetry.BoundingBox.Contains(MouseEarthCoordinate)) throw new AnalysisPointLocationException("Analysis Points cannot be placed outside the bathymetry bounds.");
                var analysisPoint = new AnalysisPoint(MouseEarthCoordinate);
#if false
            var analysisPointViewModel = new AnalysisPointCalculationPreviewViewModel
            {
                AnalysisPoint = analysisPoint,
            };
#endif

                var distinctModes = (from platform in _experiment.NemoFile.Scenario.Platforms
                                     from source in platform.Sources
                                     from mode in source.Modes
                                     select mode).Distinct();
                foreach (var mode in distinctModes)
                {
                    analysisPoint.SoundSources.Add(new SoundSource(analysisPoint, mode, 16));
#if false
                var transmissionLossJobViewModel = new TransmissionLossJobViewModel(MouseEarthCoordinate, mode, 16, 3000)
                                                   {
                                                       Name = string.Format("{0}", mode.PSMName),
                                                       IDField = _experiment.NextObjectID,
                                                   };
                try
                {
                    analysisPointViewModel.AnalysisPoint.SoundSources.Add(transmissionLossJobViewModel.TransmissionLossJob.SoundSource);
                    //var cassRunFile = CassRunFile.Create(transmissionLossJobViewModel.TransmissionLossJob, environmentInformation, transmissionLossSettings, _experiment.NemoFile.Scenario.TimeFrame);
                    //cassRunFile.Save(Path.GetDirectoryName(_experiment.FileName));
                    var ramRunFile = TransmissionLossRunFile.Create(TransmissionLossAlgorithm.RAM, transmissionLossJobViewModel.TransmissionLossJob, environmentInformation, transmissionLossSettings);
                    ramRunFile.Save(_experiment.LocalStorageRoot);
                    var bellhopRunFile = TransmissionLossRunFile.Create(TransmissionLossAlgorithm.Bellhop, transmissionLossJobViewModel.TransmissionLossJob, environmentInformation, transmissionLossSettings);
                    bellhopRunFile.Save(_experiment.LocalStorageRoot);
                    analysisPointViewModel.TransmissionLossJobViewModels.Add(transmissionLossJobViewModel);
                    analysisPointViewModel.TransmissionLossRunFiles.Add(bellhopRunFile);
                }
                catch (BathymetryOutOfBoundsException ex)
                {
                    _dispatcher.InvokeIfRequired(() => _messageBoxService.ShowError("Unable to add analysis point.\nDid you click outside the bounds of the simulation area?\n\n" + ex.Message));
                    _dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow));
                    return;
                }
                catch (BathymetryTooShallowException ex)
                {
                    _dispatcher.InvokeIfRequired(() => _messageBoxService.ShowError("This area is too shallow to place an analysis point.  Pick a different area.\n\n" + ex.Message));
                    _dispatcher.InvokeIfRequired(() => MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow));
                    return;
                }
#endif
                }
                var analysisPointSettingsViewModel = new AnalysisPointSettingsViewModel(analysisPoint);
                var settingsResult = _visualizerService.ShowDialog("AnalysisPointSettingsView", analysisPointSettingsViewModel);
                IsInAnalysisPointMode = false;
                if ((!settingsResult.HasValue) || (!settingsResult.Value))
                    return;
                var maxRadial = analysisPoint.SoundSources.Where(s => s.ShouldBeCalculated).Aggregate(0, (current, soundSource) => Math.Max(current, soundSource.Radius));
                //for (var i = 0; i < 360; i += 90) if (!_experiment.Bathymetry.BoundingBox.Contains(new EarthCoordinate(MouseEarthCoordinate, i, maxRadial))) throw new AnalysisPointLocationException("One or more radial endpoints extends beyond the bounds of the bathymetry.");

                MediatorMessage.Send(MediatorMessage.AddAnalysisPoint, analysisPoint);
#if false
            var result = _visualizerService.ShowDialog("AnalysisPointCalculationPreviewView", analysisPointViewModel);
            if ((!result.HasValue) || (!result.Value))
            {
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
                return;
            }
            MediatorMessage.Send(MediatorMessage.AddAnalysisPoint, analysisPointViewModel.AnalysisPoint);
            if (_bellhopQueueCalculatorViewModel == null)
            {
                _bellhopQueueCalculatorViewModel = new BellhopQueueCalculatorViewModel(_experiment.LocalStorageRoot, _messageBoxService);
                _visualizerService.Show("BellhopQueueCalculatorView", _bellhopQueueCalculatorViewModel, false, null);
            }

            var backgroundWorker = new BackgroundWorker();
            backgroundWorker.DoWork += delegate
                                        {
                                            foreach (var bellhopRunFile in analysisPointViewModel.TransmissionLossRunFiles) 
                                                _dispatcher.BeginInvoke(new MediatorSendDelegate(MediatorMessage.Send), DispatcherPriority.Background, MediatorMessage.QueueTransmissionLossJob, bellhopRunFile);
                                        };
            backgroundWorker.RunWorkerAsync();
#endif
            }
            catch (Exception e)
            {
                _messageBoxService.ShowError("Error placing Analysis Point: " + e.Message);
            }
            finally
            {
                MediatorMessage.Send(MediatorMessage.SetMapCursor, Cursors.Arrow);
            }
        }

        [MediatorMessageSink(MediatorMessage.EditAnalysisPoint)]
        void EditAnalysisPoint(AnalysisPoint analysisPoint)
        {
            var analysisPointSettingsViewModel = new AnalysisPointSettingsViewModel(analysisPoint);
            var settingsResult = _visualizerService.ShowDialog("AnalysisPointSettingsView", analysisPointSettingsViewModel);
            if (settingsResult.HasValue && settingsResult.Value)
            {
                ScenarioMetadata.MapLayers.DisplayAnalysisPoint(analysisPoint);
                MediatorMessage.Send(MediatorMessage.RefreshMap, true);
            }
            analysisPoint.Validate();
        }

        [MediatorMessageSink(MediatorMessage.RemoveAnalysisPoint)]
        void RemoveAnalysisPoint(AnalysisPoint analysisPoint)
        {
            ScenarioMetadata.AnalysisPoints.Remove(analysisPoint);
        }
    }
}
