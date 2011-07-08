using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Metadata;
using ESME.TransmissionLoss.CASS;

namespace ESME.Overlay
{
    public abstract class NAEMODescriptors<T> : List<KeyValuePair<string, T>> where T : NAEMODescriptor, new()
    {
        public delegate KeyValuePair<string, T> NewDescriptor(string sourceFilename);

        public delegate IEnumerable<string> FilenameFilter(IEnumerable<string> fileName); 

        protected NAEMODescriptors(string selectedRangeComplexName, string subDirectoryName, string searchPattern, FilenameFilter filenameFilter = null)
        {
            if (string.IsNullOrEmpty(selectedRangeComplexName)) return;
            Console.WriteLine("{0} Entered NAEMODescriptors constructor", DateTime.Now);
            var files = Directory.GetFiles(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, selectedRangeComplexName, subDirectoryName), searchPattern);
            Console.WriteLine("{0} Got directory listing containing {1} files", DateTime.Now, files.Length);
            IEnumerable<string> filteredFiles = files;
            if (filenameFilter != null)
            {
                filteredFiles = filenameFilter(files);
                Console.WriteLine("{0} Filtered directory listing, now contains {1} files", DateTime.Now, filteredFiles.Count());
            }
            Console.WriteLine("{0} About to call AddRange", DateTime.Now);
            AddRange(filteredFiles.Select(file => new KeyValuePair<string, T>(Path.GetFileNameWithoutExtension(file), new T { DataFilename = file })));
            Console.WriteLine("{0} Leaving NAEMODescriptors constructor", DateTime.Now);
        }

        public virtual NAEMODescriptor this[string overlayKey]
        {
            get { return this.FirstOrDefault(f => f.Key == overlayKey).Value; }
        }
    }

    public class NAEMOOverlayDescriptors : NAEMODescriptors<NAEMOOverlayDescriptor>
    {
        public NAEMOOverlayDescriptors(string selectedRangeComplexName)
            : base(selectedRangeComplexName, "Areas", "*.ovr")
        {
            foreach (var ovrItem in this)
            {
                if (ovrItem.Value.Metadata == null)
                {
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
                    var sourceOverlayFile = Path.Combine(Path.GetDirectoryName(ovrItem.Value.DataFilename), sourceOverlay + ".ovr");
                    if (File.Exists(sourceOverlayFile))
                    {
                        float bufferSize;
                        var bufferIsValid = float.TryParse(buffer.Substring(0, buffer.Length - 2), out bufferSize);
                        if (!bufferIsValid)
                        {
                            bufferSize = 0;
                            sourceOverlay = null;
                        }
                        ovrItem.Value.Metadata = new NAEMOOverlayMetadata
                                           {
                                               Filename = NAEMOMetadataBase.MetadataFilename(ovrItem.Value.DataFilename),
                                               SourceOverlay = sourceOverlay,
                                               BufferZoneSize = bufferSize,

                                           };
                        ovrItem.Value.Metadata.Save();
                    }
                }
            }
        }
    }

    public class NAEMOBathymetryDescriptors : NAEMODescriptors<NAEMOBathymetryDescriptor>
    {
        public NAEMOBathymetryDescriptors(string selectedRangeComplexName)
            : base(selectedRangeComplexName, "Bathymetry", "*.txt", Filter)
        {
            foreach (var bathyItem in this)
            {
                if (bathyItem.Value.Metadata == null)
                {
                    bathyItem.Value.Metadata = new NAEMOBathymetryMetadata()
                    {
                        Filename = NAEMOMetadataBase.MetadataFilename(bathyItem.Value.DataFilename),
                        Resolution = (float)bathyItem.Value.Data.Samples.Resolution, //very slow!
                    };
                    bathyItem.Value.Metadata.Save();
                }
            }

        }

        static IEnumerable<string> Filter(IEnumerable<string> raw)
        {
            return raw.Where(item => !item.ToLower().EndsWith("_security_readme.txt"));
        }
    }

    public class NAEMOEnvironmentDescriptors : NAEMODescriptors<NAEMOEnvironmentDescriptor>
    {
        public NAEMOEnvironmentDescriptors(string selectedRangeComplexName)
            : base(selectedRangeComplexName, "Environment", "*.dat")
        {
            foreach (var envItem in this)
            {
                if (envItem.Value.Metadata == null)
                {
                    envItem.Value.Metadata = new NAEMOEnvironmentMetadata()
                    {
                        Filename = NAEMOMetadataBase.MetadataFilename(envItem.Value.DataFilename),
                        TimePeriod = envItem.Value.Data.TimePeriod,
                    };
                    envItem.Value.Metadata.Save();
                }
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