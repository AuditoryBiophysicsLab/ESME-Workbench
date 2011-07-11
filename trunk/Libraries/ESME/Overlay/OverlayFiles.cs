using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Metadata;
using ESME.TransmissionLoss.CASS;
using HRC.Utility;

namespace ESME.Overlay
{
    public abstract class NAEMODescriptors<T> : List<System.Collections.Generic.KeyValuePair<string, T>> where T : NAEMODescriptor, new()
    {
        public delegate System.Collections.Generic.KeyValuePair<string, T> NewDescriptor(string sourceFilename);

        public delegate IEnumerable<string> FilenameFilter(IEnumerable<string> fileName); 

        protected NAEMODescriptors(string selectedRangeComplexName, string subDirectoryName, string searchPattern, FilenameFilter filenameFilter = null, BackgroundTask backgroundTask = null)
        {
            if (string.IsNullOrEmpty(selectedRangeComplexName)) return;
            //Console.WriteLine("{0} Entered NAEMODescriptors constructor", DateTime.Now);
            var files = Directory.GetFiles(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, selectedRangeComplexName, subDirectoryName), searchPattern);
            //Console.WriteLine("{0} Got directory listing containing {1} files", DateTime.Now, files.Length);
            IEnumerable<string> filteredFiles = files;
            if (filenameFilter != null)
            {
                filteredFiles = filenameFilter(files);
                //Console.WriteLine("{0} Filtered directory listing, now contains {1} files", DateTime.Now, filteredFiles.Count());
            }
            if (backgroundTask != null)
            {
                backgroundTask.Maximum = filteredFiles.Count() * 2;
                backgroundTask.Value = 0;
            }
            //Console.WriteLine("{0} About to call AddRange", DateTime.Now);
            foreach (var file in filteredFiles)
            {
                Add(new System.Collections.Generic.KeyValuePair<string, T>(Path.GetFileNameWithoutExtension(file), new T { DataFilename = file }));
                if (backgroundTask != null) backgroundTask.Value++;
            }
            //AddRange(filteredFiles.Select(file => new System.Collections.Generic.KeyValuePair<string, T>(Path.GetFileNameWithoutExtension(file), new T { DataFilename = file })));
            //Console.WriteLine("{0} Leaving NAEMODescriptors constructor", DateTime.Now);
        }

        public virtual NAEMODescriptor this[string overlayKey]
        {
            get { return this.FirstOrDefault(f => f.Key == overlayKey).Value; }
        }
    }

    public class NAEMOOverlayDescriptors : NAEMODescriptors<NAEMOOverlayDescriptor>
    {
        public NAEMOOverlayDescriptors(string selectedRangeComplexName, BackgroundTask backgroundTask = null)
            : base(selectedRangeComplexName, "Areas", "*.ovr", null, backgroundTask)
        {
            foreach (var ovrItem in this)
            {
                if ((backgroundTask != null) && (backgroundTask.CancellationPending)) return;
                if (backgroundTask != null) backgroundTask.Value++;
                if (ovrItem.Value.Metadata != null) continue;
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

    public class NAEMOBathymetryDescriptors : NAEMODescriptors<NAEMOBathymetryDescriptor>
    {
        public NAEMOBathymetryDescriptors(string selectedRangeComplexName, BackgroundTask backgroundTask = null)
            : base(selectedRangeComplexName, "Bathymetry", "*.txt", Filter, backgroundTask)
        {
            foreach (var bathyItem in this)
            {
                if ((backgroundTask != null) && (backgroundTask.CancellationPending)) return;
                if (backgroundTask != null) backgroundTask.Value++;
                if (bathyItem.Value.Metadata != null) continue;
                Bathymetry bathymetry;
                bathyItem.Value.Metadata = NAEMOBathymetryMetadata.FromBathymetryFile(bathyItem.Value.DataFilename, out bathymetry);
                bathyItem.Value.Metadata.Save();
            }

        }

        static IEnumerable<string> Filter(IEnumerable<string> raw)
        {
            return raw.Where(item => !item.ToLower().EndsWith("_security_readme.txt"));
        }
    }

    public class NAEMOEnvironmentDescriptors : NAEMODescriptors<NAEMOEnvironmentDescriptor>
    {
        public NAEMOEnvironmentDescriptors(string selectedRangeComplexName, BackgroundTask backgroundTask = null)
            : base(selectedRangeComplexName, "Environment", "*.dat", null, backgroundTask)
        {
            foreach (var envItem in this)
            {
                if ((backgroundTask != null) && (backgroundTask.CancellationPending)) return;
                if (backgroundTask != null) backgroundTask.Value++;
                if (envItem.Value.Metadata != null) continue;
                envItem.Value.Metadata = NAEMOEnvironmentMetadata.FromEnvironmentFile(envItem.Value.DataFilename);
                if (envItem.Value.Metadata == null) continue;
                envItem.Value.Metadata.Save();
            }
        }
    }

    public abstract class NAEMODescriptor
    {
        public abstract void Save();
        public string DataFilename { get; set; }
    }

    public abstract class NAEMODescriptor<TData, TMetadata> : NAEMODescriptor where TMetadata : NAEMOMetadataBase, new()
    {

        public abstract TData Data { get; }
        TMetadata _metadata;
        public TMetadata Metadata
        {
            get { return _metadata ?? (_metadata = NAEMOMetadataBase.Load<TMetadata>(DataFilename)); }
            set { _metadata = value; }
        }

        public override void Save() { Metadata.Save(Metadata); }
    }

    public class NAEMOOverlayDescriptor : NAEMODescriptor<OverlayFile, NAEMOOverlayMetadata>
    {
        #region public OverlayFile Data { get; set; }

        public override OverlayFile Data
        {
            get { return _data ?? (_data = new OverlayFile(DataFilename)); }
        }
        OverlayFile _data;

        #endregion
    }

    public class NAEMOBathymetryDescriptor : NAEMODescriptor<Bathymetry, NAEMOBathymetryMetadata>
    {
        #region public OverlayFile Data { get; set; }

        public override Bathymetry Data
        {
            get { return _data ?? (_data = Bathymetry.FromYXZ(DataFilename,-1)); }
        }
        Bathymetry _data;

        #endregion
    }

    public class NAEMOEnvironmentDescriptor : NAEMODescriptor<NAEMOEnvironmentFile, NAEMOEnvironmentMetadata>
    {
        #region public OverlayFile Data { get; set; }

        public override NAEMOEnvironmentFile Data
        {
            get { return _data ?? (_data = NAEMOEnvironmentFile.Load(DataFilename)); }
        }
        NAEMOEnvironmentFile _data;

        #endregion
    }

}