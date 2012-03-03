using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Navigation;

namespace ESME.Databases
{
#if false
    public class Simulation
    {
        public int SimulationID { get; set; }
        public TimeSpan TimeStep { get; set; }

        public virtual ICollection<SimulationSpecies> Species { get; set; }
        public virtual ICollection<SimulationPlatform> Platforms { get; set; }
        public virtual ICollection<TimeStep> TimeSteps { get; set; }
    }

    public class Actor
    {
        public int ActorID { get; set; }
        public Platform Platform { get; set; }
        public Species Species { get; set; }
    }

    public class SimulationSpecies : Species
    {
        public virtual ICollection<Actor> Animats { get; set; }
    }

    public class SimulationPlatform : Platform
    {
        public virtual ICollection<Actor> Platforms { get; set; }
    }

    public class Species
    {
        public int SpeciesID { get; set; }
        public string CommonName { get; set; }
        public string LatinName { get; set; }
        public string DefinitionFile { get; set; }
    }

    public class TimeStep
    {
        public int TimeStepID { get; set; }
        public TimeSpan SimulationTime { get; set; }

        public virtual ICollection<TimeStepLocation> TimeStepLocations { get; set; }
    }

    public class Boundary
    {
        public int BoundaryID { get; set; }
        public string Name { get; set; }

        public virtual ICollection<GeoDatum> BoundaryPoints { get; set; }
    }

    public class GeoDatum
    {
        public int GeoDatumID { get; set; }
        public double X { get; set; }
        public double Y { get; set; }
        public double Z { get; set; }
    }

    public class TimeStepLocation
    {
        public long TimeStepLocationID { get; set; }
        public float Latitude { get; set; }
        public float Longitude { get; set; }
        public float Depth { get; set; }

        public virtual TimeStep TimeStep { get; set; }
        public virtual Actor Actor { get; set; }
    }

    // Written into the binary file, NOT into the database
    public class Exposure
    {
        public int TimeStepID { get; set; }
        public int SourceActorID { get; set; }
        public int DestinationActorID { get; set; }
        public float PeakPressure { get; set; }
        public float Energy { get; set; }
    }

    public class ActiveMode
    {
        public long ActiveModeID { get; set; }

        public virtual Mode Mode { get; set; }
    }
    public class Threshold
    {
        public int ThresholdID { get; set; }
        public float Frequency { get; set; }
        public float Value { get; set; }
    }
#endif
}
