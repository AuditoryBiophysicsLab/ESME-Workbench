﻿using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Windows;
using ESME;
using ESME.Behaviors;
using ESME.Environment;
using ESME.Locations;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.Views.Controls;
using ESME.Views.Locations;
using ESME.Views.Scenarios;
using ESMEWorkbench.ViewModels.Map;
using ESMEWorkbench.ViewModels.Tree;
using HRC;
using HRC.Aspects;
using HRC.Navigation;
using HRC.ViewModels;
using HRC.WPF;

namespace ESMEWorkbench.ViewModels.Main
{
    public partial class MainViewModel
    {
        Scenario _scenario;

        [Initialize] public LayerTreeViewModel LayerTreeViewModel { get; set; }
        public MapViewModel MapViewModel { get; set; }

        [Affects("IsScenarioLoaded", "CanPlaceAnalysisPoint")] 
        public Scenario Scenario
        {
            get { return _scenario; }
            set
            {
                if (_scenario != null)
                {
                    if (Database.Context.IsModified)
                    {
                        var result = _messageBox.ShowYesNoCancel(string.Format("The database has been modified.  Would you like to save your changes before {0}?", value == null ? "closing this experiment": "switching experiments"), MessageBoxImage.Question);
                        switch (result)
                        {
                            case MessageBoxResult.Yes:
                                Database.SaveChanges();
                                break;
                            case MessageBoxResult.Cancel:
                                return;
                        }
                    }
                    _scenario.RemoveMapLayers();
                }
                _scenario = value;
                LayerTreeViewModel.Scenario = _scenario;
                MainWindowTitle = string.Format("ESME Workbench: {0}", _scenario == null ? "<No scenario loaded>" : _scenario.Name);
                if (_scenario == null) return;
                _cache[_scenario.Wind].ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() => _scenario.Wind.CreateMapLayers()));
                _cache[_scenario.SoundSpeed].ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() => _scenario.SoundSpeed.CreateMapLayers()));
                _cache[_scenario.Bathymetry].ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() => _scenario.Bathymetry.CreateMapLayers()));
                _cache[_scenario.Sediment].ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() => _scenario.Sediment.CreateMapLayers()));

                _scenario.CreateMapLayers();
                _scenario.Location.LayerSettings.IsChecked = true;
                MediatorMessage.Send(MediatorMessage.SetMapExtent, (GeoRect)_scenario.Location.GeoRect);
                MediatorMessage.Send(MediatorMessage.RefreshMap, true);
            }
        }

        public bool IsScenarioLoaded { get { return Scenario != null; } }

        public bool CanPlaceAnalysisPoint
        {
            get
            {
                if (Scenario == null) return false;
                if (Scenario.Platforms.Count == 0) return false;
                var modes = (from platform in Scenario.Platforms from source in platform.Sources from mode in source.Modes select mode).ToList();
                if (modes.Count == 0) return false;
                return Scenario.Wind != null && Scenario.SoundSpeed != null && Scenario.Bathymetry != null && Scenario.Sediment != null;
            }
        }

        public bool IsInAnalysisPointMode { get; set; }

        public string MainWindowTitle { get; set; }

        [MediatorMessageSink(MediatorMessage.LoadScenario), UsedImplicitly]
        void LoadScenario(Scenario scenario) { Scenario = scenario; }

        [MediatorMessageSink(MediatorMessage.DeleteAllScenarios), UsedImplicitly]
        void DeleteAllScenarios(Location location)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to delete all scenarios in location \"{0}\"?", location.Name), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            foreach (var scenario in location.Scenarios.ToList()) scenario.Delete();
        }

        [MediatorMessageSink(MediatorMessage.DeleteScenario), UsedImplicitly]
        void DeleteScenario(Scenario scenario)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to the scenario \"{0}\"?", scenario.Name), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            scenario.Delete();
        }

        #region CreateScenarioCommand
        public SimpleCommand<object, EventToCommandArgs> CreateScenarioCommand { get { return _createScenario ?? (_createScenario = new SimpleCommand<object, EventToCommandArgs>(o => IsCreateScenarioCommandEnabled, o => CreateScenarioHandler())); } }
        SimpleCommand<object, EventToCommandArgs> _createScenario;
        bool IsCreateScenarioCommandEnabled { get { return Database.Context.Locations.Local.Count > 0; } }

        Location _lastCreateScenarioLocation;
        [MediatorMessageSink(MediatorMessage.CreateScenario)]
        void CreateScenarioHandler(Location location = null)
        {
            if (_lastCreateScenarioLocation == null) _lastCreateScenarioLocation = Database.Context.Locations.Local.First();
            var vm = new CreateScenarioViewModel { Locations = Database.Context.Locations.Local, PluginManager = _plugins, Location = location ?? _lastCreateScenarioLocation, TimePeriod = (TimePeriod)DateTime.Today.Month, IsLocationSelectable = location == null };
            var result = _visualizer.ShowDialog("CreateScenarioView", vm);
            if ((!result.HasValue) || (!result.Value)) return;
            if (location == null) _lastCreateScenarioLocation = vm.Location;
            var scenario = CreateScenario(vm.Location, vm.ScenarioName, vm.Comments, vm.TimePeriod, vm.SelectedPlugins[PluginSubtype.Wind].SelectedDataSet, vm.SelectedPlugins[PluginSubtype.SoundSpeed].SelectedDataSet, vm.SelectedPlugins[PluginSubtype.Bathymetry].SelectedDataSet, vm.SelectedPlugins[PluginSubtype.Sediment].SelectedDataSet);
            Scenario = scenario;
#if false
            var wind = Database.LoadOrCreateEnvironmentalDataSet(vm.Location,
                                                                 vm.SelectedPlugins[PluginSubtype.Wind].SelectedDataSet.Resolution,
                                                                 vm.TimePeriod,
                                                                 vm.SelectedPlugins[PluginSubtype.Wind].SelectedDataSet.SourcePlugin);
            var soundSpeed = Database.LoadOrCreateEnvironmentalDataSet(vm.Location,
                                                                       vm.SelectedPlugins[PluginSubtype.SoundSpeed].SelectedDataSet.Resolution,
                                                                       vm.TimePeriod,
                                                                       vm.SelectedPlugins[PluginSubtype.SoundSpeed].SelectedDataSet.SourcePlugin);
            var bathymetry = Database.LoadOrCreateEnvironmentalDataSet(vm.Location,
                                                                       vm.SelectedPlugins[PluginSubtype.Bathymetry].SelectedDataSet.Resolution,
                                                                       TimePeriod.Invalid,
                                                                       vm.SelectedPlugins[PluginSubtype.Bathymetry].SelectedDataSet.SourcePlugin);
            var sediment = Database.LoadOrCreateEnvironmentalDataSet(vm.Location,
                                                                     vm.SelectedPlugins[PluginSubtype.Sediment].SelectedDataSet.Resolution,
                                                                     TimePeriod.Invalid,
                                                                     vm.SelectedPlugins[PluginSubtype.Sediment].SelectedDataSet.SourcePlugin);
            var scenario = new Scenario
            {
                Wind = wind,
                SoundSpeed = soundSpeed,
                Bathymetry = bathymetry,
                Sediment = sediment,
                Name = vm.ScenarioName,
                Location = vm.Location,
                Comments = vm.Comments,
                TimePeriod = vm.TimePeriod,
            };
            vm.Location.Scenarios.Add(scenario);
            Database.Add(scenario);
            Database.SaveChanges();
#endif
        }

        Scenario CreateScenario(Location location, string scenarioName, string comments, TimePeriod timePeriod, EnvironmentalDataSet wind, EnvironmentalDataSet soundSpeed, EnvironmentalDataSet bathymetry, EnvironmentalDataSet sediment)
        {
            var scenario = new Scenario
            {
                Wind = Database.LoadOrCreateEnvironmentalDataSet(location, wind.Resolution, timePeriod, wind.SourcePlugin),
                SoundSpeed = Database.LoadOrCreateEnvironmentalDataSet(location, soundSpeed.Resolution, timePeriod, soundSpeed.SourcePlugin),
                Bathymetry = Database.LoadOrCreateEnvironmentalDataSet(location, bathymetry.Resolution, TimePeriod.Invalid, bathymetry.SourcePlugin),
                Sediment = Database.LoadOrCreateEnvironmentalDataSet(location, sediment.Resolution, TimePeriod.Invalid, sediment.SourcePlugin),
                Name = scenarioName,
                Location = location,
                Comments = comments,
                TimePeriod = timePeriod,
            };
            location.Scenarios.Add(scenario);
            Database.Add(scenario);
            return scenario;
        }
        #endregion

        [MediatorMessageSink(MediatorMessage.DeleteAnalysisPoint), UsedImplicitly]
        void DeleteAnalysisPoint(AnalysisPoint analysisPoint)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to delete this analysis point \"{0}\"?", analysisPoint.Geo), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            //analysisPoint.LayerSettings.IsChecked = false;
            //analysisPoint.RemoveMapLayers();
            //await TaskEx.Delay(50);
            analysisPoint.Delete();
        }

        [MediatorMessageSink(MediatorMessage.DeleteAllAnalysisPoints), UsedImplicitly]
        void DeleteAllAnalysisPoints(bool dummy)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to delete all analysis points from the scenario {0} ?", Scenario.Name), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            //foreach (var analysisPoint in Scenario.AnalysisPoints) analysisPoint.LayerSettings.IsChecked = false;
            //foreach (var analysisPoint in Scenario.AnalysisPoints) analysisPoint.RemoveMapLayers();
            //await TaskEx.Delay(50);
            foreach (var analysisPoint in Scenario.AnalysisPoints.ToList()) analysisPoint.Delete();
        }

        [MediatorMessageSink(MediatorMessage.RecalculateAnalysisPoint), UsedImplicitly]
        void RecalculateAnalysisPoint(AnalysisPoint analysisPoint)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to recalculate this analysis point \"{0}\"?", analysisPoint.Geo), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            foreach (var radial in analysisPoint.TransmissionLosses.SelectMany(tl => tl.Radials))
            {
                radial.IsCalculated = false;
                File.Delete(radial.BasePath + ".shd");
                _transmissionLoss.Add(radial);
            }
        }

        [MediatorMessageSink(MediatorMessage.ViewAnalysisPointProperties), UsedImplicitly]
        void ViewAnalysisPointProperties(AnalysisPoint analysisPoint) { _visualizer.ShowDialog("TreeViewItemPropertiesView", new AnalysisPointPropertiesViewModel { PropertyObject = analysisPoint }); }

        [MediatorMessageSink(MediatorMessage.RecalculateAllAnalysisPoints), UsedImplicitly]
        void RecalculateAllAnalysisPoints(bool dummy)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to recalculate all analysis points from the scenario {0} ?", Scenario.Name), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            foreach (var radial in from point in Scenario.AnalysisPoints from transmissionLoss in point.TransmissionLosses from radial in transmissionLoss.Radials select radial)
            {
                radial.IsCalculated = false;
                File.Delete(radial.BasePath + ".shd");
                _transmissionLoss.Add(radial);
            }
        }

        [MediatorMessageSink(MediatorMessage.DeleteTransmissionLoss), UsedImplicitly]
        void DeleteTransmissionLoss(ESME.Scenarios.TransmissionLoss transmissionLoss)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to delete this transmission loss \"{0}\"?", transmissionLoss.AnalysisPoint.Geo), MessageBoxImage.Warning) !=
                MessageBoxResult.Yes) return;
            //transmissionLoss.LayerSettings.IsChecked = false;
            //transmissionLoss.RemoveMapLayers();
            //await TaskEx.Delay(50);
            transmissionLoss.Delete();
        }

        [MediatorMessageSink(MediatorMessage.RecalculateTransmissionLoss), UsedImplicitly]
        void RecalculateTransmissionLoss(ESME.Scenarios.TransmissionLoss transmissionLoss)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to recalculate this transmission loss \"{0}\"?", transmissionLoss.AnalysisPoint.Geo), MessageBoxImage.Warning) !=
                MessageBoxResult.Yes) return;
            foreach (var radial in transmissionLoss.Radials)
            {
                radial.IsCalculated = false;
                File.Delete(radial.BasePath + ".shd");
                _transmissionLoss.Add(radial);
            }
        }

        [MediatorMessageSink(MediatorMessage.TransmissionLossLayerChanged), UsedImplicitly]
        void TransmissionLossLayerChanged(IHaveLayerSettings transmissionLoss)
        {
            _dispatcher.InvokeInBackgroundIfRequired(() =>
            {
                transmissionLoss.RemoveMapLayers();
                if (!transmissionLoss.IsDeleted) transmissionLoss.CreateMapLayers();
            });
        }

        [MediatorMessageSink(MediatorMessage.AddPlatform), UsedImplicitly]
        void AddPlatform(Scenario scenario)
        {
            if (scenario.LayerControl != null) ((LayerControl)scenario.LayerControl).Expand();
            AddPlatform(scenario, "New Platform", true);
        }

        static Platform AddPlatform(Scenario scenario, string name, bool isNew)
        {
            var platform = new Platform
            {
                Scenario = scenario,
                Course = 0,
                Depth = 0,
                Description = null,
                Geo = ((GeoRect)scenario.Location.GeoRect).Center,
                PlatformName = name,
                IsRandom = false,
                Launches = false,
                TrackType = TrackType.Stationary,
                IsNew = isNew,
            };
            scenario.Platforms.Add(platform);
            platform.CreateMapLayers();
            return platform;
        }

        [MediatorMessageSink(MediatorMessage.ViewScenarioProperties),UsedImplicitly]
        void ViewScenarioProperties(Scenario scenario) { _visualizer.ShowDialog("TreeViewItemPropertiesView", new ScenarioPropertiesViewModel { PropertyObject = scenario }); }

        [MediatorMessageSink(MediatorMessage.PlatformBoundToLayer), UsedImplicitly]
        async void PlatformBoundToLayer(Platform platform)
        {
            if (!platform.IsNew) return;
            platform.IsNew = false;
            ((LayerControl)platform.LayerControl).Select();
            await TaskEx.Delay(50);
            ((LayerControl)platform.LayerControl).Edit();
        }

        [MediatorMessageSink(MediatorMessage.DeletePlatform), UsedImplicitly]
        void DeletePlatform(Platform platform)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to delete the platform \"{0}\"?", platform.PlatformName), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            platform.Delete();
            OnPropertyChanged("CanPlaceAnalysisPoint");
        }

        [MediatorMessageSink(MediatorMessage.PlatformProperties), UsedImplicitly]
        void PlatformProperties(Platform platform)
        {
            var vm = new PropertiesViewModel { PropertyObject = platform, WindowTitle = "Platform Properties: " + platform.PlatformName };
            _visualizer.ShowDialog("PlatformPropertiesView", vm);
            //_visualizer.ShowDialog("TreeViewItemPropertiesView", new PlatformPropertiesViewModel { Platform = platform });
        }

        [MediatorMessageSink(MediatorMessage.AddSource), UsedImplicitly]
        void AddSource(Platform platform)
        {
            //var vm = new CreateSourceViewModel();
            //var result = _visualizer.ShowDialog("CreateSourceView", vm);
            //if (!result.HasValue || !result.Value) return;
            ((LayerControl)platform.LayerControl).Expand();
            AddSource(platform, "New Source", true);
        }

        static Source AddSource(Platform platform, string name, bool isNew)
        {
            var source = new Source
            {
                Platform = platform,
                SourceName = name,
                SourceType = null,
                IsNew = isNew,
            };
            platform.Sources.Add(source);
            return source;
        }

        [MediatorMessageSink(MediatorMessage.SourceBoundToLayer), UsedImplicitly]
        async void SourceBoundToLayer(Source source)
        {
            if (!source.IsNew) return;
            source.IsNew = false;
            ((LayerControl)source.LayerControl).Select();
            await TaskEx.Delay(50);
            ((LayerControl)source.LayerControl).Edit();
        }

        [MediatorMessageSink(MediatorMessage.DeleteSource), UsedImplicitly]
        void DeleteSource(Source source)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to delete the source \"{0}\"?", source.SourceName), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            source.Delete();
            OnPropertyChanged("CanPlaceAnalysisPoint");
        }

        [MediatorMessageSink(MediatorMessage.SourceProperties), UsedImplicitly]
        void SourceProperties(Source source)
        {
            //_visualizer.ShowDialog("TreeViewItemPropertiesView", new SourcePropertiesViewModel { Source = source });
            var vm = new PropertiesViewModel { PropertyObject = source, WindowTitle = "Source Properties: " + source.SourceName };
            _visualizer.ShowDialog("SourcePropertiesView", vm);
        }

        [MediatorMessageSink(MediatorMessage.AddMode), UsedImplicitly]
        void AddMode(Source source)
        {
            //var vm = new CreateModeViewModel();
            //var result = _visualizer.ShowDialog("CreateModeView", vm);
            //if (!result.HasValue || !result.Value) return;
            ((LayerControl)source.LayerControl).Expand();
            AddMode(source, "New Mode", true);
            OnPropertyChanged("CanPlaceAnalysisPoint");
        }

        static Mode AddMode(Source source, string name, bool isNew)
        {
            var mode = new Mode
            {
                ActiveTime = 1f,
                Depth = 0f,
                DepressionElevationAngle = 0f,
                HighFrequency = 1000f,
                LowFrequency = 1000f,
                MaxPropagationRadius = 25000f,
                ModeName = name,
                ModeType = null,
                PulseInterval = new TimeSpan(0, 0, 0, 30),
                PulseLength = new TimeSpan(0, 0, 0, 0, 500),
                RelativeBeamAngle = 0,
                Source = source,
                SourceLevel = 200,
                VerticalBeamWidth = 180f,
                IsNew = isNew,
            };
            source.Modes.Add(mode);
            return mode;
        }

        [MediatorMessageSink(MediatorMessage.ModeBoundToLayer), UsedImplicitly]
        async void ModeBoundToLayer(Mode mode)
        {
            if (!mode.IsNew) return;
            mode.IsNew = false;
            ((LayerControl)mode.LayerControl).Select();
            await TaskEx.Delay(50);
            ((LayerControl)mode.LayerControl).Edit();
        }

        [MediatorMessageSink(MediatorMessage.DeleteMode), UsedImplicitly]
        void DeleteMode(Mode mode)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to delete the mode \"{0}\"?", mode.ModeName), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            mode.Delete();
            OnPropertyChanged("CanPlaceAnalysisPoint");
        }

        [MediatorMessageSink(MediatorMessage.RecalculateMode), UsedImplicitly]
        void RecalculateMode(Mode mode)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to recalculate all transmission losses for the mode \"{0}\"?", mode.ModeName), MessageBoxImage.Warning) !=
                MessageBoxResult.Yes) return;
            foreach (var radial in mode.TransmissionLosses.SelectMany(tl => tl.Radials))
            {
                radial.IsCalculated = false;
                File.Delete(radial.BasePath + ".shd");
                _transmissionLoss.Add(radial);
            }
        }

        [MediatorMessageSink(MediatorMessage.ModeProperties), UsedImplicitly]
        void ModeProperties(Mode mode)
        {
            var vm = new PropertiesViewModel { PropertyObject = mode, WindowTitle = "Mode Properties: " + mode.ModeName };
            _visualizer.ShowDialog("ModePropertiesView", vm);
            //_visualizer.ShowDialog("TreeViewItemPropertiesView", new ModePropertiesViewModel() { Mode = mode, });
            mode.LowFrequency = mode.HighFrequency;
        }

        #region ImportScenarioFileCommand
        SimpleCommand<object, object> _importScenarioFile;

        public SimpleCommand<object, object> ImportScenarioFileCommand { get { return _importScenarioFile ?? (_importScenarioFile = new SimpleCommand<object, object>(ImportScenarioFileHandler)); } }

        void ImportScenarioFileHandler(object o)
        {
            var vm = new ImportScenarioFileViewModel(Database, _cache, _plugins);
            var result = _visualizer.ShowDialog("ImportScenarioFileView", vm);
            if (result.HasValue && result.Value) Scenario = vm.Scenario;
        }
        #endregion
    }
}