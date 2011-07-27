﻿using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ESME.Metadata;
using ESME.Overlay;
using HRC.Navigation;
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
                var keyName = ovrItem.Key.Split('_');
                var buffer = keyName.Last();

                float bufferZoneSize;
                if (!float.TryParse(ovrItem.Value.Data.ExpandedRange, out bufferZoneSize)) bufferZoneSize = 0;
                // If we DO have a SourceOverlay (parsed out of the comments in the overlay file), OR if we have ONLY one field (from Split()), OR if the last field does NOT end in "km"
                if (!string.IsNullOrEmpty(ovrItem.Value.Data.SourceOverlay) || keyName.Length == 1 || !buffer.ToLowerInvariant().EndsWith("km"))
                {
                    // Create a default set of metadata and save it.
                    ovrItem.Value.Metadata = new NAEMOOverlayMetadata
                    {
                        Filename = NAEMOMetadataBase.MetadataFilename(ovrItem.Value.DataFilename),
                        OverlayFilename = ovrItem.Value.Data.SourceOverlay,
                        BufferZoneSize = bufferZoneSize,
                    };
                    ovrItem.Value.Metadata.Save();
                    return;
                }

                // Otherwise, let's try to parse the expanded range out of the filename
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
            });
        }

        public void CreateNewOverlay(string rangeComplexName, string overlayName, List<EarthCoordinate> coordinates, GeoRect boundingBox, float bufferZoneSize, string sourceOverlayName)
        {
            var rangeComplexAreasFolder = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, rangeComplexName, "Areas");
            var overlayFileName = overlayName + ".ovr";
            var metadataFileName = overlayName + ".xml";
            var overlayPath = Path.Combine(rangeComplexAreasFolder, overlayFileName);
            var metadataPath = Path.Combine(rangeComplexAreasFolder, metadataFileName);
            if (coordinates != null) OverlayFile.Create(overlayPath, coordinates);
            var metadata = new NAEMOOverlayMetadata
            {
                Bounds = boundingBox,
                BufferZoneSize = bufferZoneSize,
                Filename = metadataPath,
                OverlayFilename = sourceOverlayName,
            };
            metadata.Save();
            Add(new System.Collections.Generic.KeyValuePair<string, NAEMOOverlayDescriptor>(overlayName, new NAEMOOverlayDescriptor
            {
                DataFilename = overlayPath,
                Metadata = metadata,
            }));
            Sort();
        }
    }
}