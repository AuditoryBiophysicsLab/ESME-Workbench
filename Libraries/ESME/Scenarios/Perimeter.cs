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

        OverlayShapeMapLayer _mapLayer;
        public void CreateMapLayers()
        {
            _mapLayer = new OverlayShapeMapLayer
            {
                LayerType = LayerType.OpArea,
                Name = string.Format("{0}", Guid),
                LineColor = LayerSettings.LineOrSymbolColor,
                LineWidth = (float)LayerSettings.LineOrSymbolSize,
            };
            var geos = (from p in PerimeterCoordinates
                        orderby p.Order
                        select (Geo)p.Geo).ToList();
            _mapLayer.Add(geos);
            _mapLayer.Done();
            LayerSettings.PropertyChanged += (s, e) =>
            {
                switch (e.PropertyName)
                {
                    case "IsChecked":
                        MediatorMessage.Send(LayerSettings.IsChecked ? MediatorMessage.ShowMapLayer : MediatorMessage.HideMapLayer, _mapLayer);
                        break;
                    case "LineOrSymbolColor":
                        _mapLayer.LineColor = LayerSettings.LineOrSymbolColor;
                        MediatorMessage.Send(MediatorMessage.RefreshMapLayer, _mapLayer);
                        break;
                    case "LineOrSymbolSize":
                        _mapLayer.LineWidth = (float)LayerSettings.LineOrSymbolSize;
                        MediatorMessage.Send(MediatorMessage.RefreshMapLayer, _mapLayer);
                        break;
                }
            };
        }
    }
}