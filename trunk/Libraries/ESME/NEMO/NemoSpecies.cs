using System.IO;
using System.Xml;
using ESME.Model;

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
            AnimatData = DDB.Load(Path.Combine(scenarioDirectory, "Species", SpeciesFile));
        }

        public string RecordVersion { get; private set; }
        public string SpeciesFile { get; private set; }
        public string SpeciesName { get; private set; }
        public int SpeciesCode { get; private set; }
        public int TotalAnimats { get; private set; }
        public float Population { get; private set; }
        public float TrackAreaPopulation { get; private set; }
        public int TrackAreaTotal { get; private set; }
        public float SimAreaPopulation { get; private set; }
        public int SimAreaTotal { get; private set; }
        public DDB AnimatData { get; private set; }
    }
}