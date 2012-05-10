using System;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using ESME.Locations;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Scenarios
{
    public class Perimeter : IHaveGuid, IHaveLayerSettings
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Name { get; set; }
        public virtual Scenario Scenario { get; set; }
        public virtual LayerSettings LayerSettings { get; set; }
        [Initialize]
        public virtual ObservableList<Platform> Platforms { get; set; }
        [Initialize]
        public virtual ObservableList<PerimeterCoordinate> PerimeterCoordinates { get; set; }
        [Initialize]
        public virtual ObservableList<LogEntry> Logs { get; set; }

        public void CreateMapLayers()
        {
            var mapLayer = new OverlayShapeMapLayer
            {
                LayerType = LayerType.OpArea,
                Name = string.Format("{0}", Guid),
                LineColor = LayerSettings.LineOrSymbolColor,
                LineWidth = (float)LayerSettings.LineOrSymbolSize,
            };
            var geos = (from p in PerimeterCoordinates
                        orderby p.Order
                        select (Geo)p.Geo).ToList();
            mapLayer.AddPolygon(geos);
            mapLayer.Done();
            LayerSettings.MapLayerViewModel = mapLayer;
        }

        public void RemoveMapLayers() { LayerSettings.MapLayerViewModel = null; }
    }
}