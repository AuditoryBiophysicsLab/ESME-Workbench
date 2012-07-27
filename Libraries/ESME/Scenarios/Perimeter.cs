using System;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using ESME.Behaviors;
using ESME.Locations;
using ESME.Mapping;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Scenarios
{
     [NotifyPropertyChanged]
    public class Perimeter : IHaveGuid, IHaveLayerSettings
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Name { get; set; }
        public virtual Scenario Scenario { get; set; }
        [Initialize] public virtual LayerSettings LayerSettings { get; set; }
        //[Initialize] public virtual ObservableList<Platform> Platforms { get; set; }
        [Initialize] public virtual ObservableList<PerimeterCoordinate> PerimeterCoordinates { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }
        [NotMapped] public bool IsDeleted { get; set; }

        #region DeletePerimeterCommand
        public SimpleCommand<object, EventToCommandArgs> DeletePerimeterCommand { get { return _deletePerimeter ?? (_deletePerimeter = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.DeletePerimeter, this))); } }
        SimpleCommand<object, EventToCommandArgs> _deletePerimeter;
        #endregion

        #region EditPerimeterCommand
        public SimpleCommand<object, EventToCommandArgs> EditPerimeterCommand { get { return _editPerimeter ?? (_editPerimeter = new SimpleCommand<object, EventToCommandArgs>(o => MediatorMessage.Send(MediatorMessage.EditPerimeter, this))); } }
        SimpleCommand<object, EventToCommandArgs> _editPerimeter;
        #endregion

        public void CreateMapLayers()
        {
            if (IsDeleted) return;
            if (LayerSettings.MapLayerViewModel != null) return;
            var mapLayer = new OverlayShapeMapLayer
            {
                Name = string.Format("{0}", Guid),
                LineColor = LayerSettings.LineOrSymbolColor,
                LineWidth = (float)LayerSettings.LineOrSymbolSize,
            };
            var geos = (from p in PerimeterCoordinates
                        orderby p.Order
                        select (Geo)p.Geo).ToList();
            geos.Add(new Geo(geos.First()));
            mapLayer.AddPolygon(geos);
            mapLayer.Done();
            LayerSettings.MapLayerViewModel = mapLayer;
            if (Scenario.ShowAllPerimeters) LayerSettings.IsChecked = true;
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

        public void SetPerimeterCoordinates(GeoArray geoArray)
        {
            PerimeterCoordinates.Clear();
            var order = 0;
            foreach (var geo in geoArray.Geos) PerimeterCoordinates.Add(new PerimeterCoordinate { Geo = geo, Order = order++ });
        }

        public static implicit operator GeoArray(Perimeter perimeter)
        {
            return new GeoArray(from c in perimeter.PerimeterCoordinates
                                orderby c.Order
                                select (Geo)c.Geo);
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
            IsDeleted = true;
            RemoveMapLayers();
            foreach (var platform in Scenario.Platforms.Where(platform => platform.Perimeter != null && platform.Perimeter.Guid == Guid)) 
            {
                platform.TrackType = TrackType.Stationary;
                platform.IsRandom = false;
                platform.Speed = 0;
                platform.Perimeter = null;
            }
            Scenario.Database.Context.LayerSettings.Remove(LayerSettings);
            Scenario.Perimeters.Remove(this);
            foreach (var point in PerimeterCoordinates.ToList()) Scenario.Database.Context.PerimeterCoordinates.Remove(point);
        }
    }
}