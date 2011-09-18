using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Threading.Tasks;
using Cinch;
using ESME.Environment.NAVO;
using ESME.NEMO.Overlay;
using HRC;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment.Descriptors
{
    public class RangeComplexArea : ViewModelBase
    {
        public static readonly RangeComplexArea None = new RangeComplexArea {Name = "None"};
        RangeComplexArea() { }

        RangeComplexArea(NewRangeComplex rangeComplex, string areaName, OverlayShape overlayShape)
        {
            IsEnabled = false;
            _rangeComplex = rangeComplex;
            Name = areaName;
            OverlayShape = overlayShape;
            GeoRect = new GeoRect(overlayShape.BoundingBox);
            BathymetryPath = Path.Combine(_rangeComplex.DataPath, Name);
            BathymetryFiles = RangeComplexToken.Load(Path.Combine(BathymetryPath, Name + ".token"));
            BathymetryList = BathymetryFiles.GetObservableWrapper<BathymetryFile>();
            Directory.CreateDirectory(BathymetryPath);
            UpdateAvailableBathymetry();
            IsEnabled = true;
        }

        void UpdateAvailableBathymetry()
        {
            foreach (var samplesPerDegree in AvailableSampleCountsPerDegree)
            {
                var resolution = 60.0f / samplesPerDegree;
                var fileName = string.Format("{0:0.00}min.bathymetry", resolution);
                var north = Math.Round(GeoRect.North * samplesPerDegree) / samplesPerDegree;
                var south = Math.Round(GeoRect.South * samplesPerDegree) / samplesPerDegree;
                var east = Math.Round(GeoRect.East * samplesPerDegree) / samplesPerDegree;
                var west = Math.Round(GeoRect.West * samplesPerDegree) / samplesPerDegree;
                var width = east - west;
                var height = north - south;
                var localPath = Path.Combine(BathymetryPath, fileName);
                var fileInfo = File.Exists(localPath) ? new FileInfo(localPath) : null;
                var sampleCount = (uint)Math.Round(width * samplesPerDegree * height * samplesPerDegree);

                // If the file does not exist, or it's newer than the token, or it's not in the token's list of files, 
                // or it's length is different from the one stored in the token, or it's last write time is different from the one sorted in the token.
                // If any of these things are true, we want to re-extract the file from the database
                if ((fileInfo == null) || (fileInfo.LastWriteTime > BathymetryFiles.LastWriteTime) ||
                    (BathymetryFiles[fileName] == null) || (fileInfo.Length != BathymetryFiles[fileName].FileSize) ||
                    (fileInfo.LastWriteTime != BathymetryFiles[fileName].LastWriteTime))
                {
                    var bathymetryFile = new BathymetryFile(BathymetryPath, fileName, sampleCount, GeoRect,
                                                                   EnvironmentDataType.Bathymetry, NAVOTimePeriod.Invalid, resolution);
                    BathymetryFiles[fileName] = bathymetryFile;
                    if (sampleCount <= 512000)
                        ImportBathymetry(bathymetryFile);
                }
                else ((BathymetryFile)BathymetryFiles[fileName]).Reset();
            }
        }

        #region public bool IsEnabled { get; private set; }

        public bool IsEnabled
        {
            get { return _isEnabled; }
            private set
            {
                if (_isEnabled == value) return;
                _isEnabled = value;
                NotifyPropertyChanged(IsEnabledChangedEventArgs);
            }
        }

        static readonly PropertyChangedEventArgs IsEnabledChangedEventArgs = ObservableHelper.CreateArgs<RangeComplexArea>(x => x.IsEnabled);
        bool _isEnabled;

        #endregion

        internal static RangeComplexArea Create(NewRangeComplex rangeComplex, string areaName, IEnumerable<Geo> limits)
        {
            var areaPath = Path.Combine(rangeComplex.AreasPath, areaName + ".ovr");
            if (File.Exists(areaPath)) throw new InvalidOperationException(string.Format("Area {0} overlay already exists", areaName));
            if (Directory.Exists(Path.Combine(rangeComplex.DataPath, areaName))) Directory.Delete(Path.Combine(rangeComplex.DataPath, areaName), true);
            OverlayFile.Create(areaPath, limits);
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea(rangeComplex, areaName, overlay.Shapes[0]);
        }

        internal static RangeComplexArea Read(NewRangeComplex rangeComplex, string areaName)
        {
            var areaPath = Path.Combine(rangeComplex.AreasPath, areaName + ".ovr");
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea(rangeComplex, areaName, overlay.Shapes[0]);
        }

        public void ImportBathymetry(BathymetryFile bathymetryFile)
        {
            var jobDescriptor = new ImportJobDescriptor
            {
                DataType = EnvironmentDataType.Bathymetry,
                GeoRect = GeoRect,
                DestinationFilename = Path.Combine(BathymetryPath, bathymetryFile.FileName),
                Resolution = bathymetryFile.Resolution,
                CompletionFunction = arg =>
                {
                    var job = (ImportJobDescriptor)arg;
                    var thisFile = (BathymetryFile)BathymetryFiles[bathymetryFile.FileName];
                    thisFile.GeoRect = job.GeoRect;
                    thisFile.SampleCount = job.SampleCount;
                    thisFile.IsCached = true;
                    thisFile.Reset();
                    BathymetryFiles[bathymetryFile.FileName] = thisFile;
                    return job;
                }
            };
            jobDescriptor.CompletionTask = new Task<ImportJobDescriptor>(jobDescriptor.CompletionFunction, jobDescriptor);
            _rangeComplex.QueueImportJob(jobDescriptor);
        }

        public void RemoveBathymetry(BathymetryFile bathymetryFile)
        {
            if (bathymetryFile.SampleCount < 512000) throw new InvalidOperationException("This bathymetry file may not be removed.  You may, however, choose to re-import it");
            var thisFile = (BathymetryFile)BathymetryFiles[bathymetryFile.FileName];
            File.Delete(Path.Combine(BathymetryPath, bathymetryFile.FileName));
            thisFile.IsCached = false;
            thisFile.Reset();
            BathymetryFiles[bathymetryFile.FileName] = thisFile;
        }

        internal void Remove()
        {
            File.Delete(Path.Combine(_rangeComplex.AreasPath, Name + ".ovr"));
            if (Directory.Exists(BathymetryPath)) Directory.Delete(BathymetryPath, true);
        }

        public void Dump()
        {
            Debug.WriteLine("{0} Dump of area {1}\\{2}", DateTime.Now, _rangeComplex.Name, Name);
            Debug.WriteLine("{0}   Dictionary", DateTime.Now);
            foreach (var item in BathymetryFiles) Debug.WriteLine("{0}     [{1}]  size: {2}", DateTime.Now, item.Key, item.Value.FileSize);
        }

        public BathymetryFile this[string resolutionString]
        {
            get { return (BathymetryFile)BathymetryFiles[resolutionString]; }
        }

        [NotNull] public ObservableList<BathymetryFile> BathymetryList { get; private set; }

        [NotNull] public string Name { get; private set; }
        [NotNull] public string BathymetryPath { get; private set; }
        [NotNull] public GeoRect GeoRect { get; private set; }
        [NotNull] public OverlayShape OverlayShape { get; private set; }

        [NotNull] public RangeComplexToken BathymetryFiles { get; private set; }
        [NotNull] readonly NewRangeComplex _rangeComplex;

        static readonly List<uint> AvailableSampleCountsPerDegree = new List<uint> { 30, 60, 120, 600, 1200 };
    }
}