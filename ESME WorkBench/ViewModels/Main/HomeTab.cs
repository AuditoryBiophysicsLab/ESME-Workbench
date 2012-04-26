using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Windows;
using ESME;
using ESME.Environment;
using ESME.Mapping;
using ESME.Model;
using ESME.Scenarios;
using ESME.Views.Locations;
using ESMEWorkbench.ViewModels.Map;
using ESMEWorkbench.ViewModels.Tree;
using HRC.Aspects;
using HRC.Navigation;
using HRC.ViewModels;

namespace ESMEWorkbench.ViewModels.Main
{
    public partial class MainViewModel
    {
        public MapLayerCollection ScenarioMapLayers { get; set; }

        [Initialize] public LayerTreeViewModel LayerTreeViewModel { get; set; }
        public MapViewModel MapViewModel { get; set; }

        [Affects("IsScenarioLoaded")]
        public Scenario Scenario
        {
            get { return _scenario; }
            set
            {
                _scenario = value;
                LayerTreeViewModel.Scenario = _scenario;
                MainWindowTitle = string.Format("ESME Workbench: {0}", _scenario == null ? "<No scenario loaded>" : _scenario.Name   );
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
        Scenario _scenario;

        public bool IsScenarioLoaded { get { return Scenario != null; } }

        public bool CanPlaceAnalysisPoint { get { return Scenario != null && Scenario.Wind != null && Scenario.SoundSpeed != null & Scenario.Bathymetry != null & Scenario.Sediment != null; } }

        public string MainWindowTitle { get; set; }

        readonly List<Tuple<IHaveProperties, Window>> _openPropertyWindows = new List<Tuple<IHaveProperties, Window>>();
        [MediatorMessageSink(MediatorMessage.ShowProperties)]
        public void ShowProperties(IHaveProperties propertyViewModel)
        {
            var target = _openPropertyWindows.Find(property => property.Item1 == propertyViewModel);
            if (target == null)
            {
                var window = _visualizer.ShowWindow(propertyViewModel.PropertyViewName, propertyViewModel, true, (s, e) => _openPropertyWindows.Remove(_openPropertyWindows.Find(property => property.Item1 == (IHaveProperties)e.State)));
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
        public SimpleCommand<object, object> ImportScenarioFileCommand
        {
            get { return _importScenarioFile ?? (_importScenarioFile = new SimpleCommand<object, object>(ImportScenarioFileHandler)); }
        }

        SimpleCommand<object, object> _importScenarioFile;

        void ImportScenarioFileHandler(object o)
        {
            var vm = new ImportScenarioFileViewModel(Database, _cache, _plugins);
            var result = _visualizer.ShowDialog("ImportScenarioFileView", vm);
            if (result.HasValue && result.Value) Scenario = vm.Scenario;
        }
        #endregion

    }
}