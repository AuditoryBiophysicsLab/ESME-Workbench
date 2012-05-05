using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Windows;
using ESME;
using ESME.Environment;
using ESME.Mapping;
using ESME.Model;
using ESME.Plugins;
using ESME.Scenarios;
using ESME.Views.Locations;
using ESME.Views.Scenarios;
using ESMEWorkbench.ViewModels.Map;
using ESMEWorkbench.ViewModels.Tree;
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
                _scenario = value;
                LayerTreeViewModel.Scenario = _scenario;
                MainWindowTitle = string.Format("ESME Workbench: {0}", _scenario == null ? "<No scenario loaded>" : _scenario.Name);
                if (_scenario != null)
                {
                    Debug.WriteLine(string.Format("Wind contains {0} samples", ((Wind)_cache[_scenario.Wind])[_scenario.TimePeriod].EnvironmentData.Count));
                    Debug.WriteLine(string.Format("SoundSpeed contains {0} samples", ((SoundSpeed)_cache[_scenario.SoundSpeed])[_scenario.TimePeriod].EnvironmentData.Count));
                    Debug.WriteLine(string.Format("Bathymetry contains {0} samples", ((Bathymetry)_cache[_scenario.Bathymetry]).Samples));
                    Debug.WriteLine(string.Format("Sediment contains {0} samples", ((Sediment)_cache[_scenario.Sediment]).Samples));
                    //if (_scenario.Bathymetry != null) TaskEx.Run(() => { var bathy = _cache[_scenario.Bathymetry]; });
                    //if (_scenario.SoundSpeed != null) TaskEx.Run(() => { var soundSpeed = _cache[_scenario.SoundSpeed]; });
                    //if (_scenario.Sediment != null) TaskEx.Run(() => { var sediment = _cache[_scenario.Sediment]; });
                    //if (_scenario.Wind != null) TaskEx.Run(() => { var wind = _cache[_scenario.Wind]; });
                    _scenario.CreateMapLayers();
                    MediatorMessage.Send(MediatorMessage.SetMapExtent, (GeoRect)_scenario.Location.GeoRect);
                }
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
            //var parameter = args.CommandParameter;
            var vm = new CreateScenarioViewModel { Locations = Database.Context.Locations.Local, PluginManager = _plugins, Location = Database.Context.Locations.Local.First(), TimePeriod = (TimePeriod)DateTime.Today.Month };
            var result = _visualizer.ShowDialog("CreateScenarioView", vm);
            if ((!result.HasValue) || (!result.Value)) return;
            Database.Add(new Scenario
            {
                Wind = vm.SelectedPlugins[PluginSubtype.Wind].SelectedDataSet,
                SoundSpeed = vm.SelectedPlugins[PluginSubtype.SoundSpeed].SelectedDataSet,
                Bathymetry = vm.SelectedPlugins[PluginSubtype.Bathymetry].SelectedDataSet,
                Sediment = vm.SelectedPlugins[PluginSubtype.Sediment].SelectedDataSet,
                Name = vm.ScenarioName,
                Location = vm.Location,
                Comments = vm.Comments,
                TimePeriod = vm.TimePeriod,
            });
        }
        #endregion

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
            Database.Add(new AnalysisPoint { Geo = MouseGeo, Scenario = Scenario }, (Bathymetry)_cache[Scenario.Bathymetry], true);
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