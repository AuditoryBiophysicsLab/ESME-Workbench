using System.Collections.Generic;
using System.IO;
using System.Xml;
using ESME.Animats;

namespace ESME.NEMO
{
    public class NemoSpecies : NemoBase
    {
        public NemoSpecies(XmlNode species, string scenarioDirectory) : base(species) 
        {
            RecordVersion = GetString("recordVersion");
            SpeciesFile = GetString("speciesFile");
            SpeciesName = GetString("speciesName");
            SpeciesCode = GetInt("speciesCode");
            TotalAnimats = GetInt("totalAnimats");
            Population = GetInt("population");
            TrackAreaPopulation = GetFloat("trackAreaPopulation");
            TrackAreaTotal = GetInt("trackAreaTotal");
            SimAreaPopulation = GetFloat("simAreaPopulation");
            SimAreaTotal = GetInt("simAreaTotal");
            string fileSpeciesName;
            AnimatData = AnimatFile.Load(Path.Combine(scenarioDirectory, "Species", SpeciesFile),out fileSpeciesName);
        }

        public override IEnumerable<KeyValuePair<string, string>> Properties
        {
            get
            {
                yield return new KeyValuePair<string, string>("Record version", RecordVersion);
                yield return new KeyValuePair<string, string>("Species file", SpeciesFile);
                yield return new KeyValuePair<string, string>("Species name", SpeciesName);
                yield return new KeyValuePair<string, string>("Species code", SpeciesCode.ToString());
                yield return new KeyValuePair<string, string>("Total animats", TotalAnimats.ToString());
                yield return new KeyValuePair<string, string>("Population", Population.ToString());
                yield return new KeyValuePair<string, string>("Track area population", TrackAreaPopulation.ToString());
                yield return new KeyValuePair<string, string>("Track area total", TrackAreaTotal.ToString());
                yield return new KeyValuePair<string, string>("Sim area population", SimAreaPopulation.ToString());
                yield return new KeyValuePair<string, string>("Sim area total", SimAreaTotal.ToString());
            }
        }

        public string RecordVersion         { get; private set; }
        public string SpeciesFile           { get; private set; }
        public string SpeciesName           { get; private set; }
        public int    SpeciesCode           { get; private set; }
        public int    TotalAnimats          { get; private set; }
        public float  Population            { get; private set; }
        public float  TrackAreaPopulation   { get; private set; }
        public int    TrackAreaTotal        { get; private set; }
        public float  SimAreaPopulation     { get; private set; }
        public int    SimAreaTotal          { get; private set; }
        public AnimatFile AnimatData { get; private set; }
    }
}