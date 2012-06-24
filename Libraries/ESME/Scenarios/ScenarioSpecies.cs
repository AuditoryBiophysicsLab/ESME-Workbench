using System;
using System.ComponentModel.DataAnnotations;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Locations;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;
using ThinkGeo.MapSuite.Core;

namespace ESME.Scenarios
{
    public class ScenarioSpecies : IHaveGuid, IHaveLayerSettings,IEquatable<ScenarioSpecies>
    {
        public ScenarioSpecies() { SpeciesFilename = MasterDatabaseService.RandomFilenameWithoutExension + ".ani"; }
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string SpeciesFile { get; set; }
        public string LatinName { get; set; }

        public virtual Scenario Scenario { get; set; }
        [Initialize] public virtual LayerSettings LayerSettings { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }
        public string SpeciesFilename { get; set; }
        [NotMapped] public bool IsDeleted { get; set; }
        [NotMapped]
        public string SpeciesFilePath
        {
            get
            {
                if (_speciesFilePath != null) return _speciesFilePath;
                if (Scenario == null || Scenario.StorageDirectoryPath == null) throw new ApplicationException("Scenario or Scenario.StorageDirectoryPath is null");
                _speciesFilePath = Path.Combine(Scenario.StorageDirectoryPath, SpeciesFilename);
                return _speciesFilePath;
            }
        }
        string _speciesFilePath;

        #region Layer Move commands
        #region MoveLayerToFrontCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToFrontCommand { get { return _moveLayerToFront ?? (_moveLayerToFront = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerToFront(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToFront;
        #endregion

        #region MoveLayerForwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerForwardCommand { get { return _moveLayerForward ?? (_moveLayerForward = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerForward(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerForward;
        #endregion

        #region MoveLayerBackwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerBackwardCommand { get { return _moveLayerBackward ?? (_moveLayerBackward = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerBackward(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerBackward;
        #endregion

        #region MoveLayerToBackCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToBackCommand { get { return _moveLayerToBack ?? (_moveLayerToBack = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerToBack(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToBack;
        #endregion
        #endregion

        protected static readonly Random Random = new Random();
        public void CreateMapLayers()
        {
            var pointLayer = new OverlayShapeMapLayer
            {
                Name = string.Format("{0}", Guid),
                PointSymbolType = (PointSymbolType)(Random.Next(8)),
            };
            while (pointLayer.PointSymbolType == PointSymbolType.Cross) pointLayer.PointSymbolType = (PointSymbolType)(Random.Next(8));
            pointLayer.PointStyle = MapLayerViewModel.CreatePointStyle(pointLayer.PointSymbolType, LayerSettings.LineOrSymbolColor, (int)LayerSettings.LineOrSymbolSize);
            var animats = File.Exists(SpeciesFilePath) ? Animat.Load(this, SpeciesFilePath) : Animat.Seed(this, 0.01, Scenario.Location.GeoRect, Scenario.BathymetryData);
            pointLayer.Clear();
            pointLayer.AddPoints(animats.Locations.Select(l => new Geo(l.Latitude, l.Longitude)).ToList());
            pointLayer.Done();
            LayerSettings.MapLayerViewModel = pointLayer;
            LayerSettings.IsChecked = true;
        }
        public void RemoveMapLayers() { LayerSettings.MapLayerViewModel = null; }
        public void Delete()
        {
            IsDeleted = true;
            Scenario.Database.Context.LayerSettings.Remove(LayerSettings);
            Scenario.Database.Context.ScenarioSpecies.Remove(this);
        }
        
        public bool Equals(ScenarioSpecies other) { return Guid == other.Guid; }
    }
}