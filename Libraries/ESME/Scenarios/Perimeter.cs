using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using ESME.Locations;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Navigation;

namespace ESME.Scenarios
{
    public class Perimeter : IHaveGuid, IHaveLayerSettings
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Name { get; set; }
        public virtual Scenario Scenario { get; set; }
        public virtual LayerSettings LayerSettings { get; set; }
        public virtual ICollection<Platform> Platforms { get; set; }
        public virtual ICollection<PerimeterCoordinate> PerimeterCoordinates { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }

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
            mapLayer.Add(geos);
            mapLayer.Done();
            LayerSettings.MapLayerViewModel = mapLayer;
        }

        public void RemoveMapLayers() { LayerSettings.MapLayerViewModel = null; }
    }
}