using System;
using System.ComponentModel.DataAnnotations;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Windows.Data;
using ESME.Database;
using ESME.Environment;
using ESME.Mapping;
using ESME.Plugins;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using ThinkGeo.MapSuite.Core;

namespace ESME.Locations
{
    public class EnvironmentalDataSet : IHaveGuid, IHaveLayerSettings
    {
        public EnvironmentalDataSet() { TimePeriod = Environment.TimePeriod.Invalid; }
        [Key, Initialize]
        public Guid Guid { get; set; }
        public float Resolution { get; set; }
        public int SampleCount { get; set; }
        public long FileSize { get; set; }

        public DbTimePeriod TimePeriod { get; set; }
        
        public string FileName { get; set; }
        public DbPluginIdentifier SourcePlugin { get; set; }

        public virtual Location Location { get; set; }
        [Initialize] public virtual LayerSettings LayerSettings { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }

        [NotMapped]
        public string LayerName
        {
            get
            {
                switch (((PluginIdentifier)SourcePlugin).PluginSubtype)
                {
                    case PluginSubtype.Wind:
                        return "Wind Speed";
                    case PluginSubtype.SoundSpeed:
                        return "Sound Speed";
                    case PluginSubtype.Bathymetry:
                        return "Bathymetry";
                    case PluginSubtype.Sediment:
                        return "Sediment Type";
                    default:
                        throw new ApplicationException(string.Format("Unknown layer type: {0}", ((PluginIdentifier)SourcePlugin).PluginSubtype));
                }
            }
        }
        [NotMapped] public bool IsDeleted { get; set; }

        protected static readonly Random Random = new Random();
        public void CreateMapLayers()
        {
            var dataType = ((PluginIdentifier)SourcePlugin).PluginSubtype;
            switch (dataType)
            {
                case PluginSubtype.SoundSpeed:
                    var pointLayer = new OverlayShapeMapLayer
                    {
                        Name = string.Format("{0}", Guid),
                        PointSymbolType = (PointSymbolType)(Random.Next(8)),
                    };
                    while (pointLayer.PointSymbolType == PointSymbolType.Cross) pointLayer.PointSymbolType = (PointSymbolType)(Random.Next(8));
                    pointLayer.PointStyle = MapLayerViewModel.CreatePointStyle(pointLayer.PointSymbolType, LayerSettings.LineOrSymbolColor, (int)LayerSettings.LineOrSymbolSize);
                    var geos = (from s in ((SoundSpeed)Location.Cache[this].Result).SoundSpeedFields[0].EnvironmentData
                                select (Geo)s).ToArray();
                    pointLayer.Clear();
                    pointLayer.AddPoints(geos);
                    pointLayer.Done();
                    LayerSettings.MapLayerViewModel = pointLayer;
                    break;
                case PluginSubtype.Wind:
                case PluginSubtype.Bathymetry:
                case PluginSubtype.Sediment:
                    var rasterLayer = new RasterMapLayer
                    {
                        Name = string.Format("{0}", Guid),
                        North = (float)Location.GeoRect.North,
                        South = (float)Location.GeoRect.South,
                        East = (float)Location.GeoRect.East,
                        West = (float)Location.GeoRect.West,
                        RasterFilename = Path.Combine(Location.Database.MasterDatabaseDirectory, Location.StorageDirectory, Path.GetFileNameWithoutExtension(FileName) + ".bmp"),
                    };
                    LayerSettings.MapLayerViewModel = rasterLayer;
                    break;
                default:
                    throw new ApplicationException(string.Format("Unknown layer type: {0}", ((PluginIdentifier)SourcePlugin).PluginSubtype));
            }
        }
        public void RemoveMapLayers() { LayerSettings.MapLayerViewModel = null; }

    }

    public class EnvironmentalDataSetGroupByTypeConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            var environmentalDataSet = value as EnvironmentalDataSet;
            return environmentalDataSet != null ? (environmentalDataSet).LayerName : null;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture) { throw new NotImplementedException(); }
    }

}