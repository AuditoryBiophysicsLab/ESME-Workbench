using System.Xml;

namespace ESME.NEMO
{
    public class NemoSpecies : NemoBase
    {
        public NemoSpecies(XmlNode species, string scenarioDirectory)
            : base(species)
        {
            SpeciesFile = GetString("speciesFile");
        }

        public string SpeciesFile { get; private set; }
    }
}