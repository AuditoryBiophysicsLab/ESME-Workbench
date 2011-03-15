using System.Collections.Generic;
using System.Linq;
using System.Xml;

namespace ESME.NEMO
{
    public class NemoAnimals : NemoBase
    {
        public List<NemoSpecies> Species { get; private set; }
        
        public NemoAnimals(XmlNode animals, string scenarioDirectory)
        {
            Species = new List<NemoSpecies>();
            foreach (var cur in animals.ChildNodes.Cast<XmlNode>().Where(cur => cur.Name == "Species"))
            {
                Species.Add(new NemoSpecies(cur, scenarioDirectory));
            }
        }
    }
}