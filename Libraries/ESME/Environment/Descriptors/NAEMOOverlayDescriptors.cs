using System.Linq;
using System.Threading.Tasks;
using ESME.Metadata;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public class NAEMOOverlayDescriptors : NAEMODescriptors<NAEMOOverlayDescriptor>
    {
        public NAEMOOverlayDescriptors(string selectedRangeComplexName) : base(selectedRangeComplexName, "Areas", "*.ovr") { Refresh(); }

        public new void Refresh(BackgroundTask backgroundTask = null)
        {
            Parallel.ForEach(this, ovrItem =>
            {
                if ((backgroundTask != null) && (backgroundTask.CancellationPending)) return;
                if (backgroundTask != null) backgroundTask.Value++;
                // If we already have metadata, do nothing
                if ((ovrItem.Value == null) || (ovrItem.Value.Metadata != null)) return;
                // If we don't have metadata, we want to create it from the overlay file if possible
                AddOverlayDescriptor(ovrItem);
            });
        }

        public void AddOverlayDescriptor(System.Collections.Generic.KeyValuePair<string, NAEMOOverlayDescriptor> overlayDescriptor)
        {
            var keyName = overlayDescriptor.Key.Split('_');
            var buffer = keyName.Last();

            // If we have more than one field (from Split()), OR if the last field does NOT end in "km"
            if (keyName.Length == 1 || !buffer.ToLowerInvariant().EndsWith("km"))
            {
                // Create a default set of metadata and save it.
                overlayDescriptor.Value.Metadata = new NAEMOOverlayMetadata
                {
                    Filename = NAEMOMetadataBase.MetadataFilename(overlayDescriptor.Value.DataFilename),
                };
                overlayDescriptor.Value.Metadata.Save();
                return;
            }

            // Otherwise, let's try to parse the expanded range out of the filename
            var bufferStart = overlayDescriptor.Key.IndexOf(buffer) - 1;
            var sourceOverlay = overlayDescriptor.Key.Substring(0, bufferStart);
            float bufferSize;
            var bufferIsValid = float.TryParse(buffer.Substring(0, buffer.Length - 2), out bufferSize);
            if (!bufferIsValid) bufferSize = 0;
            overlayDescriptor.Value.Metadata = new NAEMOOverlayMetadata
            {
                Filename = NAEMOMetadataBase.MetadataFilename(overlayDescriptor.Value.DataFilename),
                OverlayFilename = sourceOverlay,
                BufferZoneSize = bufferSize,
            };
            overlayDescriptor.Value.Metadata.Save();
        }
    }
}