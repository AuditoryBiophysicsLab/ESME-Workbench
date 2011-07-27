﻿using System.Linq;
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

            float bufferZoneSize;
            if (!float.TryParse(overlayDescriptor.Value.Data.ExpandedRange, out bufferZoneSize)) bufferZoneSize = 0;
            // If we DO have a SourceOverlay (parsed out of the comments in the overlay file), OR if we have ONLY one field (from Split()), OR if the last field does NOT end in "km"
            if (!string.IsNullOrEmpty(overlayDescriptor.Value.Data.SourceOverlay) || keyName.Length == 1 || !buffer.ToLowerInvariant().EndsWith("km"))
            {
                // Create a default set of metadata and save it.
                overlayDescriptor.Value.Metadata = new NAEMOOverlayMetadata
                {
                    Filename = NAEMOMetadataBase.MetadataFilename(overlayDescriptor.Value.DataFilename),
                    OverlayFilename = overlayDescriptor.Value.Data.SourceOverlay,
                    BufferZoneSize = bufferZoneSize,
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