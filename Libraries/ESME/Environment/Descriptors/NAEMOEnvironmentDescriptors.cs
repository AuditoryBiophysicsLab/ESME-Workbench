using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ESME.Metadata;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public class NAEMOEnvironmentDescriptors : NAEMODescriptors<NAEMOEnvironmentDescriptor>
    {
        public NAEMOEnvironmentDescriptors() {  }

        public NAEMOEnvironmentDescriptors(string selectedRangeComplexName) : base(selectedRangeComplexName, "Environment", "*.dat") { Refresh(); }

        public new void Refresh(BackgroundTask backgroundTask = null)
        {
            Parallel.ForEach(this, envItem =>
            {
                if ((backgroundTask != null) && (backgroundTask.CancellationPending)) return;
                if (backgroundTask != null) backgroundTask.Value++;
                if ((envItem.Value == null) || (envItem.Value.Metadata != null)) return;
                envItem.Value.Metadata = NAEMOEnvironmentMetadata.FromEnvironmentFile(envItem.Value.DataFilename);
                if (envItem.Value.Metadata == null) return;
                envItem.Value.Metadata.Save();
            });
        }

        public IEnumerable<NAEMOEnvironmentDescriptor> GetDependentEnvironments(IEnumerable<NAEMOOverlayDescriptor> overlayDescriptors, IEnumerable<NAEMOBathymetryDescriptor> bathymetryDescriptors)
        {
            var step1 = new List<NAEMOEnvironmentDescriptor>();
            foreach (var overlayDescriptor in overlayDescriptors) step1.AddRange(GetDependentEnvironments(overlayDescriptor));
            var step2 = new List<NAEMOEnvironmentDescriptor>();
            foreach (var bathymetryDescriptor in overlayDescriptors) step1.AddRange(GetDependentEnvironments(bathymetryDescriptor));
            step2.AddRange(step1);
            var result = step2.Distinct().ToList();
            result.Sort();
            return result;
        }

        public IEnumerable<NAEMOEnvironmentDescriptor> GetDependentEnvironments(NAEMOOverlayDescriptor sourceOverlayDescriptor)
        {
            var sourceOverlayName = Path.GetFileNameWithoutExtension(sourceOverlayDescriptor.DataFilename);
            var result = (from environmentDescriptor in this
                          where ((environmentDescriptor.Value != null) && (Path.GetFileNameWithoutExtension(environmentDescriptor.Value.Metadata.OverlayFilename) == sourceOverlayName))
                          select environmentDescriptor.Value).Distinct().ToList();
            result.Sort();
            return result;
        }

        public IEnumerable<NAEMOEnvironmentDescriptor> GetDependentEnvironments(IEnumerable<NAEMOBathymetryDescriptor> bathymetryDescriptors)
        {
            var step1 = new List<NAEMOEnvironmentDescriptor>();
            foreach (var bathymetryDescriptor in bathymetryDescriptors) step1.AddRange(GetDependentEnvironments(bathymetryDescriptor));
            var result = step1.Distinct().ToList();
            result.Sort();
            return result;
        }

        public IEnumerable<NAEMOEnvironmentDescriptor> GetDependentEnvironments(NAEMOBathymetryDescriptor sourceBathymetryDescriptor)
        {
            var sourceBathymetryName = Path.GetFileNameWithoutExtension(sourceBathymetryDescriptor.DataFilename);
            //foreach (var cur in this)
            //{
            //    if (cur.Value == null) continue;
            //    if (Path.GetFileNameWithoutExtension(cur.Value.Metadata.BathymetryName) == sourceBathymetryName) Console.WriteLine("Matched " + sourceBathymetryName + " in " + Path.GetFileNameWithoutExtension(cur.Value.Metadata.Filename));
            //}
            var result = (from environmentDescriptor in this
                          where ((environmentDescriptor.Value != null) && (environmentDescriptor.Value.Metadata.BathymetryName == sourceBathymetryName))
                          select environmentDescriptor.Value).Distinct().ToList();
            result.Sort();
            return result;
        }

        public void DeleteEnvironment(IEnumerable<NAEMOEnvironmentDescriptor> descriptors)
        {
            foreach (var descriptor in descriptors) DeleteEnvironment(descriptor);
        }

        public void DeleteEnvironment(NAEMOEnvironmentDescriptor descriptor)
        {
            var listEntry = Find(item => (item.Value != null) && (item.Value.DataFilename == descriptor.DataFilename));
            Remove(listEntry);
            File.Delete(descriptor.DataFilename);
            File.Delete(descriptor.Metadata.Filename);
        }

    }
}