using System.Threading.Tasks;
using ESME.Metadata;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public class NAEMOEnvironmentDescriptors : NAEMODescriptors<NAEMOEnvironmentDescriptor>
    {
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
    }
}