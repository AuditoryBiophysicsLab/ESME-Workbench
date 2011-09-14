using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ESME.Environment.NAVO;
using ESME.NEMO.Overlay;
using HRC;
using HRC.Navigation;

namespace ESME.Environment.Descriptors
{
    public class RangeComplexArea
    {
        private RangeComplexArea(NewRangeComplex rangeComplex, string areaName, OverlayShape overlayShape, EnvironmentFileList<Bathymetry> bathymetryFiles,  RangeComplexToken token)
        {
            _rangeComplex = rangeComplex;
            Name = areaName;
            OverlayShape = overlayShape;
            BathymetryFiles = bathymetryFiles ?? new EnvironmentFileList<Bathymetry>();
            _token = token;
            GeoRect = new GeoRect(overlayShape.BoundingBox);
            BathymetryPath = Path.Combine(_rangeComplex.DataPath, Name);
            Directory.CreateDirectory(BathymetryPath);
            ImportJobs = new List<ImportJobDescriptor>();
            UpdateAvailableBathymetry();
        }

        void UpdateAvailableBathymetry()
        {
            foreach (var samplesPerDegree in AvailableSampleCountsPerDegree)
            {
                var resolution = 60.0f / samplesPerDegree;
                var resolutionString = string.Format("{0:0.00}min", resolution);
                var north = Math.Round(GeoRect.North * samplesPerDegree) / samplesPerDegree;
                var south = Math.Round(GeoRect.South * samplesPerDegree) / samplesPerDegree;
                var east = Math.Round(GeoRect.East * samplesPerDegree) / samplesPerDegree;
                var west = Math.Round(GeoRect.West * samplesPerDegree) / samplesPerDegree;
                var width = east - west;
                var height = north - south;
                var isDataAvailable = File.Exists(Path.Combine(BathymetryPath, resolutionString + ".bathymetry"));
                var sampleCount = (uint)Math.Round(width * samplesPerDegree * height * samplesPerDegree);
                if ((isDataAvailable) || (sampleCount >= 512000)) continue;
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
                        var bathymetryName = Path.Combine(Name, jobResolution + ".bathymetry");
                        var envFile = new EnvironmentFile<Bathymetry>(_rangeComplex.DataPath, bathymetryName, bathyJob.SampleCount, bathyJob.GeoRect, EnvironmentDataType.Bathymetry, NAVOTimePeriod.Invalid);
                        BathymetryFiles.Add(jobResolution, envFile);
                        _token.EnvironmentDictionary[bathymetryName] = envFile;
                    },
                });
            }
        }

        internal static RangeComplexArea Create(NewRangeComplex rangeComplex, string areaName, IEnumerable<Geo> limits, RangeComplexToken token)
        {
            var areaPath = Path.Combine(rangeComplex.AreasPath, areaName + ".ovr");
            OverlayFile.Create(areaPath, limits);
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea(rangeComplex, areaName, overlay.Shapes[0], null, token);
        }

        internal static RangeComplexArea Read(NewRangeComplex rangeComplex, string areaName, EnvironmentFileList<Bathymetry> files, RangeComplexToken token)
        {
            var areaPath = Path.Combine(rangeComplex.AreasPath, areaName + ".ovr");
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea(rangeComplex, areaName, overlay.Shapes[0], files, token);
        }

        public Bathymetry this[string resolutionString]
        {
            get { return BathymetryFiles[resolutionString].Data; }
        }

        public Task<Bathymetry> GetDataAsync(string resolutionString)
        {
            return BathymetryFiles[resolutionString].AsyncData;
        }

        public List<ImportJobDescriptor> ImportJobs { get; private set; }

        [NotNull] public EnvironmentFileList<Bathymetry> BathymetryFiles { get; private set; }
        [NotNull] public string Name { get; private set; }
        [NotNull] public string BathymetryPath { get; private set; }
        [NotNull] public GeoRect GeoRect { get; private set; }
        [NotNull] public OverlayShape OverlayShape { get; private set; }

        [NotNull] readonly RangeComplexToken _token;
        [NotNull] readonly NewRangeComplex _rangeComplex;

        static readonly List<uint> AvailableSampleCountsPerDegree = new List<uint> { 30, 60, 120, 600, 1200 };
    }
}