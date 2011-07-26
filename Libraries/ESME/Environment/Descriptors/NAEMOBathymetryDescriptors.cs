using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using ESME.Metadata;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public class NAEMOBathymetryDescriptors : NAEMODescriptors<NAEMOBathymetryDescriptor>
    {
        public NAEMOBathymetryDescriptors(string selectedRangeComplexName, BackgroundTask backgroundTask = null)
                : base(selectedRangeComplexName, "Bathymetry", "*.txt", Filter, backgroundTask)
        {
            Parallel.ForEach(this, bathyItem =>
            {
                if ((backgroundTask != null) && (backgroundTask.CancellationPending)) return;
                if (backgroundTask != null) backgroundTask.Value++;
                if ((bathyItem.Value == null) ||
                    ((bathyItem.Value.Metadata != null) && (bathyItem.Value.Metadata.PointCount != 0))) return;

                Bathymetry bathymetry;
                bathyItem.Value.Metadata = NAEMOBathymetryMetadata.FromBathymetryFile(bathyItem.Value.DataFilename,
                                                                                      out bathymetry);
                bathyItem.Value.Metadata.Save();
            });

        }

        static IEnumerable<string> Filter(IEnumerable<string> raw)
        {
            return raw.Where(item => !item.ToLower().EndsWith("_security_readme.txt"));
        }
    }
}