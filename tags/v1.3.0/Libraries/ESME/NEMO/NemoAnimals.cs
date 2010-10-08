using System.Collections.Generic;
using System.Linq;
using System.Xml;

namespace ESME.NEMO
{
    public class NemoAnimals : NemoBase
    {
        readonly List<NemoSpecies> _species = new List<NemoSpecies>();

        public NemoAnimals(XmlNode animals, string scenarioDirectory)
        {
            foreach (var cur in animals.ChildNodes.Cast<XmlNode>().Where(cur => cur.Name == "Species"))
            {
                _species.Add(new NemoSpecies(cur, scenarioDirectory));
            }
        }
    }
}