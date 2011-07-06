using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Metadata;
using ESME.TransmissionLoss.CASS;

namespace ESME.Overlay
{
    public class OverlayFiles : List<KeyValuePair<string, OverlayFile>>
    {
        public OverlayFiles(string selectedRangeComplexName)
        {
            if (string.IsNullOrEmpty(selectedRangeComplexName)) return;
            Clear();
            var files = Directory.GetFiles(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, selectedRangeComplexName, "Areas"), "*.ovr");
            AddRange(files.Select(file => new KeyValuePair<string, OverlayFile>(Path.GetFileNameWithoutExtension(file), new OverlayFile(file))));
        }

        public OverlayFile this[string overlayKey]
        {
            get { return this.FirstOrDefault(f => f.Key == overlayKey).Value; }
        }
    }

    public abstract class NAEMODescriptors<T> : List<KeyValuePair<string, T>> where T : NAEMODescriptor, new()
    {
        public delegate KeyValuePair<string, T> NewDescriptor(string sourceFilename);

        protected NAEMODescriptors(string selectedRangeComplexName, string subDirectoryName, string searchPattern)
        {
            if (string.IsNullOrEmpty(selectedRangeComplexName)) return;
            var files = Directory.GetFiles(Path.Combine(Globals.AppSettings.ScenarioDataDirectory, selectedRangeComplexName, subDirectoryName), searchPattern);
            AddRange(files.Select(file => new KeyValuePair<string, T>(Path.GetFileNameWithoutExtension(file), new T{DataFilename = file})));
        }

        public virtual NAEMODescriptor this[string overlayKey]
        {
            get { return this.FirstOrDefault(f => f.Key == overlayKey).Value; }
        }
    }

    public class NAEMOOverlayDescriptors : NAEMODescriptors<OverlayDescriptor>
    {
        public NAEMOOverlayDescriptors(string selectedRangeComplexName)
            : base(selectedRangeComplexName, "Areas", "*.ovr")
        { }
    }

    public class NAEMOBathymetryDescriptors : NAEMODescriptors<BathymetryDescriptor>
    {
        public NAEMOBathymetryDescriptors(string selectedRangeComplexName)
            : base(selectedRangeComplexName, "Areas", "*.ovr")
        { }
    }

    public abstract class NAEMODescriptor
    {
        public abstract void Save();
        public string DataFilename { get; set; }

        public abstract NAEMODescriptor Load(string sourceFilename);
    }

    public abstract class NAEMODescriptor<TData, TMetadata> : NAEMODescriptor where TMetadata : NAEMOMetadataBase, new()
    {

        public abstract TData Data { get; }
        TMetadata _metadata;
        public TMetadata Metadata
        {
            get { return _metadata ?? (_metadata = NAEMOMetadataBase.Load<TMetadata>(DataFilename)); }
        }

        public override void Save() { Metadata.Save(Metadata); }
        public override NAEMODescriptor Load(string sourceFilename)
        {
            throw new System.NotImplementedException();
        }
    }

    public class OverlayDescriptor : NAEMODescriptor<OverlayFile, NAEMOOverlayMetadata>
    {
        #region public OverlayFile Data { get; set; }

        public override OverlayFile Data
        {
            get { return _data ?? (_data = new OverlayFile(DataFilename)); }
        }
        OverlayFile _data;

        #endregion
    }

    public class BathymetryDescriptor : NAEMODescriptor<Bathymetry, NAEMOBathymetryMetadata>
    {
        #region public OverlayFile Data { get; set; }

        public override Bathymetry Data
        {
            get { return _data ?? (_data = Bathymetry.Load(DataFilename)); }
        }
        Bathymetry _data;

        #endregion
    }

    public class EnvironmentDescriptor : NAEMODescriptor<NAEMOEnvironmentFile, NAEMOEnvironmentMetadata>
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