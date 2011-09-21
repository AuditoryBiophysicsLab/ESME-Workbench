using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HRC.Utility;

namespace ESME.Metadata
{
#if false
    public class NAEMOMetadata : PropertyChangedBase
    {
        static readonly List<Type> ReferencedTypes = new List<Type>();
        static NAEMOMetadata()
        {
            ReferencedTypes.AddRange(NAEMOScenarioMetadata.ReferencedTypes);
            ReferencedTypes.AddRange(NAEMOBathymetryMetadata.ReferencedTypes);
            ReferencedTypes.AddRange(NAEMOEnvironmentMetadata.ReferencedTypes);
        }

        public NAEMOScenarioMetadata ScenarioMetadata { get; set; }
        public NAEMOBathymetryMetadata NaemoBathymetryMetadata { get; set; }
        public NAEMOEnvironmentMetadata EnvironmentMetadata { get; set; }


        public static NAEMOScenarioMetadata LoadScenarioMetadata(string filename) { return XmlSerializer<NAEMOScenarioMetadata>.Load(filename, ReferencedTypes); }
        public static NAEMOBathymetryMetadata LoadBathymetryMetadata(string filename) { return XmlSerializer<NAEMOBathymetryMetadata>.Load(filename, ReferencedTypes); }
        public static NAEMOEnvironmentMetadata LoadEnvironmentMetadata(string filename) { return XmlSerializer<NAEMOEnvironmentMetadata>.Load(filename, ReferencedTypes); }
    }
#endif
}
