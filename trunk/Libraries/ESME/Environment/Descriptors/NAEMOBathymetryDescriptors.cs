using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ESME.Metadata;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public class NAEMOBathymetryDescriptors : NAEMODescriptors<NAEMOBathymetryDescriptor>
    {
        public NAEMOBathymetryDescriptors(string selectedRangeComplexName) : base(selectedRangeComplexName, "Bathymetry", "*.txt", Filter) { Refresh(); }

        public new void Refresh(BackgroundTask backgroundTask = null)
        {
            base.Refresh(backgroundTask);
            Parallel.ForEach(this, bathyItem =>
            {
                if ((backgroundTask != null) && (backgroundTask.CancellationPending)) return;
                if (backgroundTask != null) backgroundTask.Value++;
                if ((bathyItem.Value == null) || ((bathyItem.Value.Metadata != null) && (bathyItem.Value.Metadata.PointCount != 0))) return;

                Bathymetry bathymetry;
                bathyItem.Value.Metadata = NAEMOBathymetryMetadata.FromBathymetryFile(bathyItem.Value.DataFilename, out bathymetry);
                bathyItem.Value.Metadata.Save();
            });
        }

        static IEnumerable<string> Filter(IEnumerable<string> raw)
        {
            return raw.Where(item => !item.ToLower().EndsWith("_security_readme.txt"));
        }

        public IEnumerable<NAEMOBathymetryDescriptor> GetDependentBathymetries(IEnumerable<NAEMOOverlayDescriptor> overlayDescriptors)
        {
            var step1 = new List<NAEMOBathymetryDescriptor>();
            foreach (var overlayDescriptor in overlayDescriptors) step1.AddRange(GetDependentBathymetries(overlayDescriptor));
            var result = step1.Distinct().ToList();
            result.Sort();
            return result;
        }

        public IEnumerable<NAEMOBathymetryDescriptor> GetDependentBathymetries(NAEMOOverlayDescriptor sourceOverlayDescriptor)
        {
            var sourceOverlayName = Path.GetFileNameWithoutExtension(sourceOverlayDescriptor.DataFilename);
            var result = (from bathymetryDescriptor in this
                          where ((bathymetryDescriptor.Value != null) && (Path.GetFileNameWithoutExtension(bathymetryDescriptor.Value.Metadata.OverlayFilename) == sourceOverlayName))
                          select bathymetryDescriptor.Value).Distinct().ToList();
            result.Sort();
            return result;
        }

        public void DeleteBathymetry(IEnumerable<NAEMOBathymetryDescriptor> bathymetryDescriptors)
        {
            foreach (var bathymetryDescriptor in bathymetryDescriptors) DeleteBathymetry(bathymetryDescriptor);
        }

        public void DeleteBathymetry(NAEMOBathymetryDescriptor bathymetryDescriptor)
        {
            var listEntry = Find(item => (item.Value != null) && (item.Value.DataFilename == bathymetryDescriptor.DataFilename));
            Remove(listEntry);
            var bathymetryName = Path.GetFileNameWithoutExtension(bathymetryDescriptor.DataFilename);
            var bathymetryPath = Path.GetDirectoryName(bathymetryDescriptor.DataFilename);
            var simAreaPath = Path.GetDirectoryName(bathymetryPath);
            var imagesPath = Path.Combine(simAreaPath, "Images");
            File.Delete(Path.Combine(imagesPath, bathymetryName + ".bmp"));
            File.Delete(Path.Combine(imagesPath, bathymetryName + ".bpw"));
            File.Delete(bathymetryDescriptor.DataFilename);
            File.Delete(bathymetryDescriptor.Metadata.Filename);
        }
    }
}