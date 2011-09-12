using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using Cinch;
using ESME.Environment.NAVO;
using ESME.NEMO.Overlay;
using HRC;
using HRC.Navigation;

namespace ESME.Environment.Descriptors
{
    public class RangeComplexArea : ViewModelBase
    {
        private RangeComplexArea(string rangeComplexPath, string areaName, OverlayShape overlayShape)
        {
            _rangeComplexPath = rangeComplexPath;
            Name = areaName;
            OverlayShape = overlayShape;
            GeoRect = new GeoRect(overlayShape.BoundingBox);
            BathymetryPath = Path.Combine(_rangeComplexPath, "Data", Name);
            Directory.CreateDirectory(BathymetryPath);
            _availableResolutions = new ObservableCollection<SampleCountTreeItem>();
            AvailableResolutions = new ReadOnlyObservableCollection<SampleCountTreeItem>(_availableResolutions);
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
                if (curItem.IsDataAvailable) 
                    Bathymetry.LoadAsync(Path.Combine(BathymetryPath, string.Format("{0:0.00}min.bathymetry", resolution))).
                        ContinueWith(task =>
                        {
                            curItem.IsDataAvailable = true;
                            curItem.SampleCount = (uint)task.Result.Samples.Count;
                            curItem.GeoRect = task.Result.Samples.GeoRect;
                        });
                else if (curItem.SampleCount < 512000) DBDB.ImportAsync(BathymetryPath, resolution, GeoRect, curItem);
                _availableResolutions.Add(curItem);
            }
        }

        internal static RangeComplexArea Create(string rangeComplexPath, string areaName, IEnumerable<Geo> limits)
        {
            var areaPath = Path.Combine(rangeComplexPath, "Areas", areaName + ".ovr");
            OverlayFile.Create(areaPath, limits);
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea(rangeComplexPath, areaName, overlay.Shapes[0]);
        }

        internal static RangeComplexArea Read(string areaPath)
        {
            var overlay = new OverlayFile(areaPath);
            return new RangeComplexArea(Path.GetDirectoryName(Path.GetDirectoryName(areaPath)), Path.GetFileNameWithoutExtension(areaPath), overlay.Shapes[0]);
        }

        [NotNull] public ReadOnlyObservableCollection<SampleCountTreeItem> AvailableResolutions { get; private set; }
        [NotNull] public string Name { get; private set; }
        [NotNull] public string BathymetryPath { get; private set; }
        [NotNull] public GeoRect GeoRect { get; private set; }
        [NotNull] public OverlayShape OverlayShape { get; private set; }

        [NotNull] readonly ObservableCollection<SampleCountTreeItem> _availableResolutions;
        [NotNull] readonly string _rangeComplexPath;

        static readonly List<uint> AvailableSampleCountsPerDegree = new List<uint> { 30, 60, 120, 600, 1200 };
    }
}