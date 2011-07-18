using System.Linq;
using ESME.Metadata;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public class NAEMOOverlayDescriptors : NAEMODescriptors<NAEMOOverlayDescriptor>
    {
        public NAEMOOverlayDescriptors(string selectedRangeComplexName, BackgroundTask backgroundTask = null)
                : base(selectedRangeComplexName, "Areas", "*.ovr", null, backgroundTask)
        {
            foreach (var ovrItem in this)
            {
                if ((backgroundTask != null) && (backgroundTask.CancellationPending)) return;
                if (backgroundTask != null) backgroundTask.Value++;
                if ((ovrItem.Value == null) || (ovrItem.Value.Metadata != null)) continue;
                var keyName = ovrItem.Key.Split('_');
                var buffer = keyName.Last();
                if (keyName.Length == 1 || !buffer.ToLowerInvariant().EndsWith("km"))
                {
                    ovrItem.Value.Metadata = new NAEMOOverlayMetadata
                    {
                            Filename = NAEMOMetadataBase.MetadataFilename(ovrItem.Value.DataFilename),
                    };
                    ovrItem.Value.Metadata.Save();
                    continue;
                }
                //now likely that range is there. 
                var bufferStart = ovrItem.Key.IndexOf(buffer) - 1;
                var sourceOverlay = ovrItem.Key.Substring(0, bufferStart);
                float bufferSize;
                var bufferIsValid = float.TryParse(buffer.Substring(0, buffer.Length - 2), out bufferSize);
                if (!bufferIsValid) bufferSize = 0;
                ovrItem.Value.Metadata = new NAEMOOverlayMetadata
                {
                        Filename = NAEMOMetadataBase.MetadataFilename(ovrItem.Value.DataFilename),
                        OverlayFilename = sourceOverlay,
                        BufferZoneSize = bufferSize,
                };
                ovrItem.Value.Metadata.Save();
            }
        }
    }
}