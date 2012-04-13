using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Windows.Media;
using ESME.Database;
using ESME.Locations;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Navigation;
using ThinkGeo.MapSuite.Core;

namespace ESME.Scenarios
{
    public class Platform : IHaveGuid, IHaveLayerSettings
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Description { get; set; }
        public bool Launches { get; set; }
        public bool Tows { get; set; }
        public int RepeatCount { get; set; }

        // Copied from the PSM Platform
        public string PSMPlatformGuid { get; set; }
        public string PlatformName { get; set; }
        public string PlatformType { get; set; }

        public DbTrackType TrackType { get; set; }
        public DbGeo Geo { get; set; }
        public bool IsRandom { get; set; }
        public float Depth { get; set; }
        public float Course { get; set; }

        /// <summary>
        /// Speed in knots (nautical miles per hour)
        /// </summary>
        public float Speed { get; set; }

        public virtual Scenario Scenario { get; set; }
        public virtual Perimeter Perimeter { get; set; }
        public virtual LayerSettings LayerSettings { get; set; }
        public virtual ICollection<Source> Sources { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }

        OverlayShapeMapLayer _mapLayer;
        public void CreateMapLayers()
        {
            _mapLayer = new OverlayShapeMapLayer
            {
                LayerType = LayerType.Track,
                Name = string.Format("{0}", Guid),
                CustomLineStyle = new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, LayerSettings.LineOrSymbolColor, (float)LayerSettings.LineOrSymbolSize)
            };

            _mapLayer.Add(new List<Geo> { Geo, ((Geo)Geo).Offset(HRC.Navigation.Geo.KilometersToRadians(25), HRC.Navigation.Geo.DegreesToRadians(90)) });
            _mapLayer.Done();
            if (Perimeter != null) Perimeter.CreateMapLayers();

            LayerSettings.PropertyChanged += (s, e) =>
            {
                switch (e.PropertyName)
                {
                    case "IsChecked":
                        MediatorMessage.Send(LayerSettings.IsChecked ? MediatorMessage.ShowMapLayer : MediatorMessage.HideMapLayer, _mapLayer);
                        break;
                    case "LineOrSymbolColor":
                    case "LineOrSymbolSize":
                        _mapLayer.CustomLineStyle = new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5,
                                                                                PointSymbolType.Square, Colors.Red, 5,
                                                                                LayerSettings.LineOrSymbolColor,
                                                                                (float)LayerSettings.LineOrSymbolSize);
                        MediatorMessage.Send(MediatorMessage.RefreshMapLayer, _mapLayer);
                        break;
                }
            };
        }

        public OverlayShapeMapLayer MapLayer
        {
            get
            {
                if (_mapLayer != null) return _mapLayer;
                _mapLayer = new OverlayShapeMapLayer
                {
                    LayerType = LayerType.Track,
                    Name = string.Format("{0}", Guid),
                    CustomLineStyle = new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5, PointSymbolType.Square, Colors.Red, 5, LayerSettings.LineOrSymbolColor, (float)LayerSettings.LineOrSymbolSize)
                };
                _mapLayer.Add(new List<Geo> { Geo, ((Geo)Geo).Offset(HRC.Navigation.Geo.KilometersToRadians(25), HRC.Navigation.Geo.DegreesToRadians(90)) });
                _mapLayer.Done();
                LayerSettings.PropertyChanged += (s, e) =>
                {
                    switch (e.PropertyName)
                    {
                        case "IsChecked":
                            MediatorMessage.Send(LayerSettings.IsChecked ? MediatorMessage.ShowMapLayer : MediatorMessage.HideMapLayer, _mapLayer);
                            break;
                        case "LineOrSymbolColor":
                        case "LineOrSymbolSize":
                            _mapLayer.CustomLineStyle = new CustomStartEndLineStyle(PointSymbolType.Circle, Colors.Green, 5,
                                                                                    PointSymbolType.Square, Colors.Red, 5,
                                                                                    LayerSettings.LineOrSymbolColor,
                                                                                    (float)LayerSettings.LineOrSymbolSize);
                            MediatorMessage.Send(MediatorMessage.RefreshMapLayer, _mapLayer);
                            break;
                    }
                };
                return _mapLayer;
            } 
        }
    }
}