using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using ESME.Database;
using ESME.Locations;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
    public class Perimeter : IHaveGuid, IHaveLayerSettings
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Name { get; set; }
        public virtual Scenario Scenario { get; set; }
        [Initialize] public virtual LayerSettings LayerSettings { get; set; }
        [Initialize] public virtual ObservableList<Platform> Platforms { get; set; }
        [Initialize] public virtual ObservableList<PerimeterCoordinate> PerimeterCoordinates { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }
        [NotMapped] public bool IsDeleted { get; set; }

        public void CreateMapLayers()
        {
            var mapLayer = new OverlayShapeMapLayer
            {
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

        public static implicit operator Perimeter(GeoArray geoArray)
        {
            var perimeter = new Perimeter();
            var geos = geoArray.Geos.ToArray();
            for (var geoIndex = 0; geoIndex < geoArray.Geos.Count(); geoIndex++)
            {
                var geo = geos[geoIndex];
                perimeter.PerimeterCoordinates.Add(new PerimeterCoordinate { Geo = geo, Order = geoIndex, Perimeter = perimeter });
            }
            return perimeter;
        }

        public static implicit operator GeoArray(Perimeter perimeter)
        {
            return new GeoArray(perimeter.PerimeterCoordinates.Select(coordinate => coordinate.Geo).Select(g => (Geo)g));
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

        public void Delete()
        {
            RemoveMapLayers();
            Scenario.Database.Context.LayerSettings.Remove(LayerSettings);
            foreach (var point in PerimeterCoordinates.ToList()) Scenario.Database.Context.PerimeterCoordinates.Remove(point);
        }
    }
}