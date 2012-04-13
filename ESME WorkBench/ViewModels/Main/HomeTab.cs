using System;
using System.Collections.Generic;
using System.Windows;
using Cinch;
using ESME;
using ESME.Environment;
using ESME.Mapping;
using ESME.Model;
using ESME.Scenarios;
using ESME.Views.Locations;
using ESMEWorkbench.ViewModels.Layers;
using ESMEWorkbench.ViewModels.Map;
using HRC.Aspects;

namespace ESMEWorkbench.ViewModels.Main
{
    public partial class MainViewModel
    {
        public MapLayerCollection ScenarioMapLayers { get; set; }

        [Initialize] public LayerTreeViewModel LayerTreeViewModel { get; set; }
        public MapViewModel MapViewModel { get; set; }

        Scenario _scenario;
        public Scenario Scenario
        {
            get { return _scenario; }
            set
            {
                _scenario = value;
                LayerTreeViewModel.Scenario = _scenario;
                MainWindowTitle = string.Format("ESME Workbench: {0}", _scenario == null ? "<No scenario loaded>" : _scenario.Name   );
                if (_scenario != null) _scenario.CreateMapLayers();
            }
        }

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