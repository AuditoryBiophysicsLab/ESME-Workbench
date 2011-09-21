using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ESME.Metadata;
using ESME.NEMO.Overlay;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
#if false
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
                        Bounds = new GeoRect(ovrItem.Value.Data.Shapes[0].EarthCoordinates),
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
                    Bounds = new GeoRect(ovrItem.Value.Data.Shapes[0].EarthCoordinates),
                };
                ovrItem.Value.Metadata.Save();
            });
        }

        public void CreateNewOverlay(string rangeComplexName, string overlayName, ICollection<Geo> coordinates, GeoRect boundingBox, float bufferZoneSize, string sourceOverlayName)
        {
            if (coordinates == null) throw new ApplicationException("Cannot create a new overlay without coordinates");
            var rangeComplexAreasFolder = Path.Combine(Globals.AppSettings.ScenarioDataDirectory, rangeComplexName, "Areas");
            var overlayFileName = overlayName + ".ovr";
            var metadataFileName = overlayName + ".xml";
            var overlayPath = Path.Combine(rangeComplexAreasFolder, overlayFileName);
            var metadataPath = Path.Combine(rangeComplexAreasFolder, metadataFileName);
            OverlayFile.Create(overlayPath, coordinates, sourceOverlayName, bufferZoneSize);
            var metadata = new NAEMOOverlayMetadata
            {
                    Filename = metadataPath,
                    OverlayFilename = sourceOverlayName,
                    BufferZoneSize = bufferZoneSize,
                    Bounds = boundingBox,
            };
            metadata.Save();
            Add(new KeyValuePair<string, NAEMOOverlayDescriptor>(overlayName,
                                                                 new NAEMOOverlayDescriptor
                                                                 {
                                                                         DataFilename =
                                                                         overlayPath,
                                                                         Metadata =
                                                                         metadata,
                                                                 }));
            Sort();
        }

        /// <summary>
        /// Returns an enumerable list of all overlays that depend on the specified overlay, yea unto many generations
        /// </summary>
        /// <param name="sourceOverlayDescriptor"></param>
        /// <returns></returns>
        public IEnumerable<NAEMOOverlayDescriptor> GetDependentOverlays(NAEMOOverlayDescriptor sourceOverlayDescriptor)
        {
            var gen1 = OverlaysDependentUpon(sourceOverlayDescriptor);
            var result = OverlaysDependentUpon(gen1).ToList();
            result.Sort();
            return result;
        }

        IEnumerable<NAEMOOverlayDescriptor> OverlaysDependentUpon(NAEMODescriptor sourceOverlayDescriptor)
        {
            var sourceOverlayName = Path.GetFileNameWithoutExtension(sourceOverlayDescriptor.DataFilename);
            return (from overlayDescriptor in this
                    where ((overlayDescriptor.Value != null) && (Path.GetFileNameWithoutExtension(overlayDescriptor.Value.Metadata.OverlayFilename) == sourceOverlayName))
                    select overlayDescriptor.Value).Distinct();
        }

        IEnumerable<NAEMOOverlayDescriptor> OverlaysDependentUpon(IEnumerable<NAEMOOverlayDescriptor> sourceOverlays)
        {
            var results = new List<NAEMOOverlayDescriptor>();
            foreach (var curOverlay in sourceOverlays) results.AddRange(OverlaysDependentUpon(curOverlay));
            var curList = results.Distinct().ToList();
            var curCount = curList.Count;
            var lastCount = 0;
            while (lastCount != curCount)
            {
                lastCount = curCount;
                results = new List<NAEMOOverlayDescriptor>();
                foreach (var item in curList) results.AddRange(OverlaysDependentUpon(item));
                curList = results.Distinct().ToList();
                curCount = curList.Count;
            }
            curList.AddRange(sourceOverlays);
            return curList;
        }

        /// <summary>
        /// Deletes all overlays in the passed-in list, without checking dependencies
        /// </summary>
        /// <param name="overlaysToDelete"></param>
        public void DeleteOverlays(IEnumerable<NAEMOOverlayDescriptor> overlaysToDelete) { foreach (var targetOverlay in overlaysToDelete) DeleteOverlay(targetOverlay); }

        void DeleteOverlay(NAEMOOverlayDescriptor targetOverlayDescriptor)
        {
            var listEntry = Find(item => (item.Value != null) && (item.Value.DataFilename == targetOverlayDescriptor.DataFilename));
            Remove(listEntry);
            File.Delete(targetOverlayDescriptor.DataFilename);
            File.Delete(targetOverlayDescriptor.Metadata.Filename);
        }
    }
#endif
}