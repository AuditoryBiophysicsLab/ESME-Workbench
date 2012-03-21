using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using ESME.Locations;
using HRC.Aspects;

namespace ESME.Scenarios
{
    public class ScenarioSpecies : IHaveGuid
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string SpeciesFile { get; set; }
        public string LatinName { get; set; }

        public virtual Scenario Scenario { get; set; }
        public virtual ICollection<AnimatLocation> AnimatLocations { get; set; }
    }
}