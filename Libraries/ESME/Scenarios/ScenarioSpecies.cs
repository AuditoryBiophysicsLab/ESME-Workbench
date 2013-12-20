using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
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
    [NotifyPropertyChanged]
    public class ScenarioSpecies : IHaveGuid, IHaveLayerSettings, IEquatable<ScenarioSpecies>
    {
        public ScenarioSpecies() { PopulationFilename = MasterDatabaseService.RandomFilenameWithoutExension + ".ani"; }
        public ScenarioSpecies(ScenarioSpecies species) : this() { Copy(species); }
        void Copy(ScenarioSpecies species)
        {
            LatinName = species.LatinName;
            SpeciesDefinitionFilename = species.SpeciesDefinitionFilename;
            LayerSettings = new LayerSettings(species.LayerSettings);
        }

        public void CopyFiles(ScenarioSpecies species)
        {
            File.Copy(species.PopulationFilePath, PopulationFilePath);
        }
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string SpeciesFile { get; set; }
        public string LatinName { get; set; }

        /// <summary>
        /// Density in animats per square kilometer for our simple seeding routine
        /// </summary>
        [Initialize(0.01f)]
        public float PopulationDensity { get; set; }

        public virtual Scenario Scenario { get; set; }
        [Initialize]
        public virtual LayerSettings LayerSettings { get; set; }
        [Initialize]
        public virtual ObservableList<LogEntry> Logs { get; set; }
        public string SpeciesDefinitionFilename { get; set; }
        public string PopulationFilename { get; set; }
        [NotMapped]
        public bool IsDeleted { get; set; }
        [NotMapped]
        public int StartActorID { get; set; }
        [NotMapped]
        public string PopulationFilePath
        {
            get
            {
                if (_populationFilePath != null) return _populationFilePath;
                if (Scenario == null || Scenario.StorageDirectoryPath == null) throw new ApplicationException("Scenario or Scenario.StorageDirectoryPath is null");
                _populationFilePath = Path.Combine(Scenario.StorageDirectoryPath, PopulationFilename);
                return _populationFilePath;
            }
        }
        string _populationFilePath;
      

        public string SpeciesDefinitionFilePath
        {
            get
            {
                if (_speciesDefinitionFilePath != null) return _speciesDefinitionFilePath;
                //if (string.IsNullOrEmpty(SpeciesDefinitionFilename)) SpeciesDefinitionFilename = "generic_odontocete.spe";
                if (string.IsNullOrEmpty(SpeciesDefinitionFilename)) throw new ApplicationException("SpeciesDefinitionFilename is not set");
                _speciesDefinitionFilePath = Path.Combine(Path.GetDirectoryName(System.Reflection.Assembly.GetCallingAssembly().Location), "Species Definition Files", SpeciesDefinitionFilename);
                if (!File.Exists(_speciesDefinitionFilePath)) throw new ApplicationException(string.Format("Species definition file \"{0}\" does not exist", _speciesDefinitionFilePath));
                return _speciesDefinitionFilePath;
            }
        }
        string _speciesDefinitionFilePath;
        [NotMapped]
        public bool IsSeededByESME { get { return PopulationFilename.ToLower().EndsWith(".ani"); } }
        Animat _animat;

        [NotMapped]
        public Animat Animat
        {
            get
            {
                if (_animat != null) return _animat;
                _animat = File.Exists(PopulationFilePath) ? Animat.Load(this, PopulationFilePath) : Animat.Seed(this, Scenario.Location.GeoRect, Scenario.BathymetryData);
                return _animat;
            }
            set { _animat = value; }
        }
        public void ReloadOrReseedAnimats() { Animat = null; }

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

        #region DeleteSpeciesCommand
        public SimpleCommand<object, EventToCommandArgs> DeleteSpeciesCommand { get { return _deleteSpecies ?? (_deleteSpecies = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeleteSpecies, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deleteSpecies;
        #endregion

        #region RepopulateSpeciesCommand
        public SimpleCommand<object, EventToCommandArgs> RepopulateSpeciesCommand { get { return _repopulateSpecies ?? (_repopulateSpecies = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.RepopulateSpecies, this))); } }
        SimpleCommand<object, EventToCommandArgs> _repopulateSpecies;
        #endregion

        #region SpeciesPropertiesCommand
        public SimpleCommand<object, EventToCommandArgs> SpeciesPropertiesCommand { get { return _speciesProperties ?? (_speciesProperties = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.SpeciesProperties, this))); } }
        SimpleCommand<object, EventToCommandArgs> _speciesProperties;
        #endregion

        protected static readonly Random Random = new Random();
        public void UpdateMapLayers()
        {
            if (IsDeleted) return;
            var mapLayer = (LayerSettings.MapLayerViewModel != null) ? (OverlayShapeMapLayer)LayerSettings.MapLayerViewModel : new OverlayShapeMapLayer { Name = string.Format("{0}", Guid) };
            mapLayer.PointSymbolType = (PointSymbolType)(Random.Next(8));
            while (mapLayer.PointSymbolType == PointSymbolType.Cross) mapLayer.PointSymbolType = (PointSymbolType)(Random.Next(8));
            mapLayer.PointStyle = MapLayerViewModel.CreatePointStyle(mapLayer.PointSymbolType, LayerSettings.LineOrSymbolColor, (int)LayerSettings.LineOrSymbolSize);
            mapLayer.Clear();
            var seededAnimats = Animat.Locations.Select(l => new Geo(l.Latitude, l.Longitude)).ToList();
            if (seededAnimats.Count == 0) throw new SpeciesSeedingException(string.Format("No individuals of the species '{0}' were successfully placed in the scenario, possibly due to water depth or other restrictions in the species file.  Please try another species.", LatinName));
            mapLayer.AddPoints(seededAnimats);
            mapLayer.Done();
            LayerSettings.MapLayerViewModel = mapLayer;
            if (Scenario.ShowAllSpecies) LayerSettings.IsChecked = true;
        }
        public void RemoveMapLayers() { LayerSettings.MapLayerViewModel = null; }

        public void Delete()
        {
            IsDeleted = true;
            RemoveMapLayers();
            Scenario.Database.Context.LayerSettings.Remove(LayerSettings);
            Scenario.Database.Context.ScenarioSpecies.Remove(this);
        }

        public bool Equals(ScenarioSpecies other) { return Guid == other.Guid; }
    }
}