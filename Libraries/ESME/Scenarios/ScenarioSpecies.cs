using System;
using System.ComponentModel.DataAnnotations;
using ESME.Locations;
using HRC.Aspects;
using HRC.Utility;

namespace ESME.Scenarios
{
    public class ScenarioSpecies : IHaveGuid, IHaveLayerSettings
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string SpeciesFile { get; set; }
        public string LatinName { get; set; }

        public virtual Scenario Scenario { get; set; }
        [Initialize] public virtual LayerSettings LayerSettings { get; set; }
        [Initialize] public virtual ObservableList<AnimatLocation> AnimatLocations { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }
        [NotMapped] public bool IsDeleted { get; set; }

        public void CreateMapLayers() { throw new NotImplementedException(); }
        public void RemoveMapLayers() { throw new NotImplementedException(); }
    }
}