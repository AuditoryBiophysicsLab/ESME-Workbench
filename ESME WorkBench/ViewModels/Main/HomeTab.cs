using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using ESME;
using ESME.Behaviors;
using ESME.Environment;
using ESME.Mapping;
using ESME.Model;
using ESME.Plugins;
using ESME.Scenarios;
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
        readonly List<Tuple<IHaveProperties, Window>> _openPropertyWindows = new List<Tuple<IHaveProperties, Window>>();

        Scenario _scenario;

        public MapLayerCollection ScenarioMapLayers { get; set; }

        [Initialize] public LayerTreeViewModel LayerTreeViewModel { get; set; }
        public MapViewModel MapViewModel { get; set; }

        [Affects("IsScenarioLoaded", "CanPlaceAnalysisPoint")] public Scenario Scenario
        {
            get { return _scenario; }
            set
            {
                if (_scenario != null)
                {
                    // todo: Remove any existing map layers here
                }
                _scenario = value;
                LayerTreeViewModel.Scenario = _scenario;
                MainWindowTitle = string.Format("ESME Workbench: {0}", _scenario == null ? "<No scenario loaded>" : _scenario.Name);
                if (_scenario == null) return;
                //Debug.WriteLine(string.Format("Wind contains {0} samples", ((Wind)_cache[_scenario.Wind].Result)[_scenario.TimePeriod].EnvironmentData.Count));
                //Debug.WriteLine(string.Format("SoundSpeed contains {0} samples", ((SoundSpeed)_cache[_scenario.SoundSpeed].Result)[_scenario.TimePeriod].EnvironmentData.Count));
                //Debug.WriteLine(string.Format("Bathymetry contains {0} samples", ((Bathymetry)_cache[_scenario.Bathymetry].Result).Samples.Count));
                //Debug.WriteLine(string.Format("Sediment contains {0} samples", ((Sediment)_cache[_scenario.Sediment].Result).Samples.Count));
                //if (_scenario.Bathymetry != null) TaskEx.Run(() => { var bathy = _cache[_scenario.Bathymetry]; });
                //if (_scenario.SoundSpeed != null) TaskEx.Run(() => { var soundSpeed = _cache[_scenario.SoundSpeed]; });
                //if (_scenario.Sediment != null) TaskEx.Run(() => { var sediment = _cache[_scenario.Sediment]; });
                //if (_scenario.Wind != null) TaskEx.Run(() => { var wind = _cache[_scenario.Wind]; });
                _cache[_scenario.Wind].ContinueWith(t => _dispatcher.InvokeInBackgroundIfRequired(() => _scenario.Wind.CreateMapLayers()));
                _cache[_scenario.SoundSpeed].ContinueWith(t =>  _dispatcher.InvokeInBackgroundIfRequired(() => _scenario.SoundSpeed.CreateMapLayers()));
                _cache[_scenario.Bathymetry].ContinueWith(t =>  _dispatcher.InvokeInBackgroundIfRequired(() => _scenario.Bathymetry.CreateMapLayers()));
                _cache[_scenario.Sediment].ContinueWith(t =>  _dispatcher.InvokeInBackgroundIfRequired(() => _scenario.Sediment.CreateMapLayers()));

                _scenario.CreateMapLayers();
                MediatorMessage.Send(MediatorMessage.SetMapExtent, (GeoRect)_scenario.Location.GeoRect);
            }
        }

        public bool IsScenarioLoaded { get { return Scenario != null; } }

        public bool CanPlaceAnalysisPoint { get { return Scenario != null && Scenario.Wind != null && Scenario.SoundSpeed != null && Scenario.Bathymetry != null && Scenario.Sediment != null; } }

        public bool IsInAnalysisPointMode { get; set; }

        public string MainWindowTitle { get; set; }

        #region CreateScenarioCommand
        SimpleCommand<object, EventToCommandArgs> _createScenario;

        public SimpleCommand<object, EventToCommandArgs> CreateScenarioCommand { get { return _createScenario ?? (_createScenario = new SimpleCommand<object, EventToCommandArgs>(o => IsCreateScenarioCommandEnabled, CreateScenarioHandler)); } }

        static bool IsCreateScenarioCommandEnabled { get { return true; } }

        void CreateScenarioHandler(EventToCommandArgs args)
        {
            var vm = new CreateScenarioViewModel { Locations = Database.Context.Locations.Local, PluginManager = _plugins, Location = Database.Context.Locations.Local.First(), TimePeriod = (TimePeriod)DateTime.Today.Month };
            var result = _visualizer.ShowDialog("CreateScenarioView", vm);
            if ((!result.HasValue) || (!result.Value)) return;
            var wind = Database.LoadOrCreateEnvironmentalDataSet(vm.Location, vm.SelectedPlugins[PluginSubtype.Wind].SelectedDataSet.Resolution, vm.TimePeriod, vm.SelectedPlugins[PluginSubtype.Wind].SelectedDataSet.SourcePlugin);
            var soundSpeed = Database.LoadOrCreateEnvironmentalDataSet(vm.Location, vm.SelectedPlugins[PluginSubtype.SoundSpeed].SelectedDataSet.Resolution, vm.TimePeriod, vm.SelectedPlugins[PluginSubtype.SoundSpeed].SelectedDataSet.SourcePlugin);
            var bathymetry = Database.LoadOrCreateEnvironmentalDataSet(vm.Location, vm.SelectedPlugins[PluginSubtype.Bathymetry].SelectedDataSet.Resolution, TimePeriod.Invalid, vm.SelectedPlugins[PluginSubtype.Bathymetry].SelectedDataSet.SourcePlugin);
            var sediment = Database.LoadOrCreateEnvironmentalDataSet(vm.Location, vm.SelectedPlugins[PluginSubtype.Sediment].SelectedDataSet.Resolution, TimePeriod.Invalid, vm.SelectedPlugins[PluginSubtype.Sediment].SelectedDataSet.SourcePlugin);
            Database.Add(new Scenario
            {
                Wind = wind,
                SoundSpeed = soundSpeed,
                Bathymetry = bathymetry,
                Sediment = sediment,
                Name = vm.ScenarioName,
                Location = vm.Location,
                Comments = vm.Comments,
                TimePeriod = vm.TimePeriod,
            }, true);
        }
        #endregion

        [MediatorMessageSink(MediatorMessage.AddPlatform), UsedImplicitly]
        void AddPlatform(Scenario scenario)
        {
            var vm = new CreatePlatformViewModel();
            var result = _visualizer.ShowDialog("CreatePlatformView", vm);
            if (!result.HasValue || !result.Value) return;
            var platform = new Platform
            {
                Scenario = scenario,
                Course = 0,
                Depth = 0,
                Description = vm.Description,
                Geo = ((GeoRect)Scenario.Location.GeoRect).Center,
                PlatformName = vm.PlatformName,
                IsRandom = false,
                Launches = false,
                TrackType = TrackType.Stationary,
            };
            Database.Add(platform, true);
        }

        [MediatorMessageSink(MediatorMessage.DeletePlatform), UsedImplicitly]
        void DeletePlatform(Platform platform)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to delete the platform \"{0}\"?", platform.PlatformName), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            Database.Context.Platforms.Remove(platform);
            Database.Context.SaveChanges();
        }
        [MediatorMessageSink(MediatorMessage.AddSource), UsedImplicitly]
        void AddSource(Platform platform)
        {
            var vm = new CreateSourceViewModel();
            var result = _visualizer.ShowDialog("CreateSourceView", vm);
            if (!result.HasValue || !result.Value) return;
            var source = new Source
            {
                Platform = platform,
                SourceName = vm.SourceName,
                SourceType = null,
            };
            Database.Add(source, true);
        }
        [MediatorMessageSink(MediatorMessage.DeleteSource), UsedImplicitly]
        void DeleteSource(Source source)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to delete the source \"{0}\"?", source.SourceName), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            Database.Context.Sources.Remove(source);
            Database.Context.SaveChanges();
        }
        [MediatorMessageSink(MediatorMessage.AddMode), UsedImplicitly]
        void AddMode(Source source)
        {
            var vm = new CreateModeViewModel();
            var result = _visualizer.ShowDialog("CreateModeView", vm);
            if (!result.HasValue || !result.Value) return;
            var mode = new Mode
            {
                ActiveTime = 1f,
                Depth = vm.Depth,
                DepressionElevationAngle = vm.DepressionElevationAngle,
                HighFrequency = vm.Frequency,
                LowFrequency = vm.Frequency,
                MaxPropagationRadius = vm.MaxPropagationRadius,
                ModeName = vm.ModeName,
                ModeType = null,
                PulseInterval = new TimeSpan(0, 0, 0, 30),
                PulseLength = new TimeSpan(0, 0, 0, 0, 500),
                RelativeBeamAngle = 0,
                Source = source,
                SourceLevel = vm.SourceLevel,
                VerticalBeamWidth = vm.VerticalBeamWidth,
            };
            Database.Add(mode, true);
        }
        [MediatorMessageSink(MediatorMessage.DeleteMode), UsedImplicitly]
        void DeleteMode(Mode mode)
        {
            if (_messageBox.ShowYesNo(string.Format("Are you sure you want to delete the mode \"{0}\"?", mode.ModeName), MessageBoxImage.Warning) != MessageBoxResult.Yes) return;
            Database.Context.Modes.Remove(mode);
            Database.Context.SaveChanges();
        }

        [MediatorMessageSink(MediatorMessage.ShowProperties)]
        public void ShowProperties(IHaveProperties propertyViewModel)
        {
            var target = _openPropertyWindows.Find(property => property.Item1 == propertyViewModel);
            if (target == null)
            {
                var window = _visualizer.ShowWindow(propertyViewModel.PropertyViewName,
                                                    propertyViewModel,
                                                    true,
                                                    (s, e) => _openPropertyWindows.Remove(_openPropertyWindows.Find(property => property.Item1 == (IHaveProperties)e.State)));
                _openPropertyWindows.Add(new Tuple<IHaveProperties, Window>(propertyViewModel, window));
            }
            else
            {
                target.Item2.Activate();
            }
        }

        [MediatorMessageSink(MediatorMessage.PlaceAnalysisPoint)]
        public void PlaceAnalysisPoint(bool dummy)
        {
            if (MouseDepth > 0) throw new AnalysisPointLocationException("Analysis Points cannot be placed on land.");
            if (Scenario == null || Scenario.Bathymetry == null) return;
            Database.Add(new AnalysisPoint { Geo = MouseGeo, Scenario = Scenario }, (Bathymetry)_cache[Scenario.Bathymetry].Result, true);
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