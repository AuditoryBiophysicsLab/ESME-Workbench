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
using HRC.ViewModels;
using HRC.WPF;
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

        [NotMapped] 
        public long FileSize
        {
            get
            {
                if (Location == null) return 0;
                if (string.IsNullOrEmpty(Location.StorageDirectoryPath)) return 0;
                if (!Directory.Exists(Location.StorageDirectoryPath)) return 0;
                if (string.IsNullOrEmpty(FileName)) return 0;
                var filePath = Path.Combine(Location.StorageDirectoryPath, FileName);
                if (!File.Exists(filePath)) return 0;
                var fi = new FileInfo(filePath);
                return fi.Length;
            }
        }

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
                    CreateRasterLayer(Path.Combine(Location.StorageDirectoryPath, Path.GetFileNameWithoutExtension(FileName) + ".bmp"), Location.GeoRect);
                    break;
                case PluginSubtype.Bathymetry:
                    CreateRasterLayer(Path.Combine(Location.StorageDirectoryPath, Path.GetFileNameWithoutExtension(FileName) + ".bmp"), ((Bathymetry)Location.Cache[this].Result).Samples.GeoRect);
                    break;
                case PluginSubtype.Sediment:
                    CreateRasterLayer(Path.Combine(Location.StorageDirectoryPath, Path.GetFileNameWithoutExtension(FileName) + ".bmp"), ((Sediment)Location.Cache[this].Result).Samples.GeoRect);
                    break;
                default:
                    throw new ApplicationException(string.Format("Unknown layer type: {0}", ((PluginIdentifier)SourcePlugin).PluginSubtype));
            }
        }

        void CreateRasterLayer(string rasterFilename, GeoRect geoRect)
        {
            if (!File.Exists(rasterFilename)) return;
            var rasterLayer = new RasterMapLayer
            {
                Name = string.Format("{0}", Guid),
                North = (float)geoRect.North,
                South = (float)geoRect.South,
                East = (float)geoRect.East,
                West = (float)geoRect.West,
                RasterFilename = rasterFilename,
            };
            LayerSettings.MapLayerViewModel = rasterLayer;
            MediatorMessage.Send(MediatorMessage.MoveLayerToBack, rasterLayer);
        }

        public void Delete()
        {
            var fileName = Path.Combine(Location.StorageDirectoryPath, FileName);
            var files = Directory.GetFiles(Path.GetDirectoryName(fileName), Path.GetFileNameWithoutExtension(fileName) + ".*");
            foreach (var file in files) File.Delete(file);
            Location.Database.Context.LayerSettings.Remove(LayerSettings);
            Location.Database.Context.EnvironmentalDataSets.Remove(this);
        }

        public void RemoveMapLayers() { LayerSettings.MapLayerViewModel = null; }
        #region Layer Move commands
        #region MoveLayerToFrontCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToFrontCommand { get { return _moveLayerToFront ?? (_moveLayerToFront = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerToFront(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToFront;
        #endregion

        #region MoveLayerForwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerForwardCommand { get { return _moveLayerForward ?? (_moveLayerForward = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerForward(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerForward;
        #endregion

        #region MoveLayerBackwardCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerBackwardCommand { get { return _moveLayerBackward ?? (_moveLayerBackward = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerBackward(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerBackward;
        #endregion

        #region MoveLayerToBackCommand
        public SimpleCommand<object, EventToCommandArgs> MoveLayerToBackCommand { get { return _moveLayerToBack ?? (_moveLayerToBack = new SimpleCommand<object, EventToCommandArgs>(o => { LayerSettings.MoveLayerToBack(); MediatorMessage.Send(MediatorMessage.RefreshMap, true); })); } }
        SimpleCommand<object, EventToCommandArgs> _moveLayerToBack;
        #endregion
        #endregion

        #region ZoomToLocationCommand
        public SimpleCommand<object, EventToCommandArgs> ZoomToLocationCommand { get { return _zoomToLocation ?? (_zoomToLocation = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.SetMapExtent, (GeoRect)Location.GeoRect))); } }
        SimpleCommand<object, EventToCommandArgs> _zoomToLocation;
        #endregion
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