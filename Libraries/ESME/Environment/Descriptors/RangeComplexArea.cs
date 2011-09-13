using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Cinch;
using ESME.NEMO.Overlay;
using HRC;
using HRC.Navigation;

namespace ESME.Environment.Descriptors
{
    public class RangeComplexArea : ViewModelBase
    {
        private RangeComplexArea(string rangeComplexPath, string areaName, OverlayShape overlayShape, EnvironmentFileList<Bathymetry> bathymetryFiles,  RangeComplexToken token)
        {
            _rangeComplexPath = rangeComplexPath;
            Name = areaName;
            OverlayShape = overlayShape;
            _bathymetryFiles = bathymetryFiles ?? new EnvironmentFileList<Bathymetry>();
            _token = token;
            GeoRect = new GeoRect(overlayShape.BoundingBox);
            BathymetryPath = Path.Combine(_rangeComplexPath, "Data", Name);
            Directory.CreateDirectory(BathymetryPath);
            _availableResolutions = new ObservableCollection<SampleCountTreeItem>();
            AvailableResolutions = new ReadOnlyObservableCollection<SampleCountTreeItem>(_availableResolutions);
            ImportJobs = new List<ImportJobDescriptor>();
            UpdateAvailableBathymetry();
        }

        void UpdateAvailableBathymetry()
        {
            _availableResolutions.Clear();
            foreach (var samplesPerDegree in AvailableSampleCountsPerDegree)
            {
                var resolution = 60.0f / samplesPerDegree;
                var resolutionString = string.Format("{0:0.00}min", resolution);
                var north = Math.Round(GeoRect.North * samplesPerDegree) / samplesPerDegree;
                var south = Math.Round(GeoRect.South * samplesPerDegree) / samplesPerDegree;
                var east = Math.Round(GeoRect.East * samplesPerDegree) / samplesPerDegree;
                var west = Math.Round(GeoRect.West * samplesPerDegree) / samplesPerDegree;
                var width = (uint)(east - west);
                var height = (uint)(north - south);
                var curItem = new SampleCountTreeItem
                {
                    Name = resolutionString,
                    IsDataAvailable = File.Exists(Path.Combine(BathymetryPath, resolutionString + ".bathymetry")),
                    SampleCount = width * samplesPerDegree * height * samplesPerDegree,
                };
                if (curItem.IsDataAvailable) curItem.Name += "!";
                if ((!curItem.IsDataAvailable) && (curItem.SampleCount < 512000))
                {
                    var bathymetryFilename = Path.Combine(BathymetryPath, string.Format("{0:0.00}min.bathymetry", resolution));
                    ImportJobs.Add(new ImportJobDescriptor
                    {
                        DataType = EnvironmentDataType.Bathymetry,
                        GeoRect = GeoRect,
                        DestinationFilename = bathymetryFilename,
                        Resolution = resolution,
                        CompletionAction = bathyJob =>
                        {
                            var jobResolution = string.Format("{0:0.00}min", bathyJob.Resolution);
                            foreach (var item in _availableResolutions.Where(item => item.Name == jobResolution))
                            {
                                item.IsDataAvailable = true;
                                item.SampleCount = bathyJob.SampleCount;
                            }
                            var bathymetryName = Path.Combine(Name, jobResolution + ".bathymetry");
                            var envFile = new EnvironmentFile<Bathymetry>(Path.Combine(_rangeComplexPath, "Data"), bathymetryName, bathyJob.SampleCount, bathyJob.GeoRect);
                            _bathymetryFiles.Add(jobResolution, envFile);
                            _token.EnvironmentDictionary[bathymetryName] = envFile;
                        },
                    });
                }
                _availableResolutions.Add(curItem);
            }
        }

        internal static RangeComplexArea Create(string rangeComplexPath, string areaName, IEnumerable<Geo> limits, RangeComplexToken token)
        {
            var areaPath = Path.Combine(rangeComplexPath, "Areas", areaName + ".ovr");
            OverlayFile.Create(areaPath, limits);
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea(rangeComplexPath, areaName, overlay.Shapes[0], null, token);
        }

        internal static RangeComplexArea Read(string areaPath, EnvironmentFileList<Bathymetry> files, RangeComplexToken token)
        {
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea(Path.GetDirectoryName(Path.GetDirectoryName(areaPath)),
                                        Path.GetFileNameWithoutExtension(areaPath), overlay.Shapes[0], files, token);
        }

        public Bathymetry this[string resolutionString]
        {
            get { return _bathymetryFiles[resolutionString].Data; }
        }

        public Task<Bathymetry> GetDataAsync(string resolutionString)
        {
            return _bathymetryFiles[resolutionString].AsyncData;
        }

        public List<ImportJobDescriptor> ImportJobs { get; private set; }

        [NotNull] public ReadOnlyObservableCollection<SampleCountTreeItem> AvailableResolutions { get; private set; }
        [NotNull] public string Name { get; private set; }
        [NotNull] public string BathymetryPath { get; private set; }
        [NotNull] public GeoRect GeoRect { get; private set; }
        [NotNull] public OverlayShape OverlayShape { get; private set; }

        [NotNull] readonly ObservableCollection<SampleCountTreeItem> _availableResolutions;
        [NotNull] readonly string _rangeComplexPath;
        [NotNull] readonly EnvironmentFileList<Bathymetry> _bathymetryFiles;
        [NotNull] readonly RangeComplexToken _token;

        static readonly List<uint> AvailableSampleCountsPerDegree = new List<uint> { 30, 60, 120, 600, 1200 };
    }
}