using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Linq;
using System.Windows.Data;
using System.Windows.Media;
using ESME.Database;
using ESME.Mapping;
using ESME.Scenarios;
using HRC.Aspects;
using HRC.Navigation;
using HRC.Utility;
using HRC.ViewModels;
using HRC.WPF;

namespace ESME.Locations
{
    public interface IHaveGuid
    {
        Guid Guid { get; }
    }

    public interface IHaveLayerSettings
    {
        LayerSettings LayerSettings { get; set; }
        void CreateMapLayers();
        void RemoveMapLayers();
        bool IsDeleted { get; }
        SimpleCommand<object, EventToCommandArgs> MoveLayerToFrontCommand { get; }
        SimpleCommand<object, EventToCommandArgs> MoveLayerForwardCommand { get; }
        SimpleCommand<object, EventToCommandArgs> MoveLayerBackwardCommand { get; }
        SimpleCommand<object, EventToCommandArgs> MoveLayerToBackCommand { get; }
    }

    public class Location : IHaveGuid, IHaveLayerSettings
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Name { get; set; }
        public string Comments { get; set; }
        public DbGeoRect GeoRect { get; set; }
        public string StorageDirectory { get; set; }

        [Initialize] public virtual LayerSettings LayerSettings { get; set; }
        [Initialize] public virtual ObservableList<Scenario> Scenarios { get; set; }
        [Initialize] public virtual ObservableList<EnvironmentalDataSet> EnvironmentalDataSets { get; set; }
        [Initialize] public virtual ObservableList<LogEntry> Logs { get; set; }

        [NotMapped]
        public CollectionViewSource DataSetTypes
        {
            get
            {
                if (_collectionView != null) return _collectionView;
                _collectionView = new CollectionViewSource {Source = EnvironmentalDataSets};
                _collectionView.GroupDescriptions.Add(new PropertyGroupDescription(null, new EnvironmentalDataSetGroupByTypeConverter()));
                return _collectionView;
            }
        }
        CollectionViewSource _collectionView;
        [NotMapped] public static IMasterDatabaseService Database { get; set; }
        [NotMapped] public static EnvironmentalCacheService Cache { get; set; }
        [NotMapped] public bool IsDeleted { get; set; }

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

        public void CreateMapLayers()
        {
            var mapLayer = new OverlayShapeMapLayer
            {
                Name = string.Format("{0}", Guid),
            };
            var geoRect = (GeoRect)GeoRect;
            mapLayer.AddPolygon(new List<Geo> { geoRect.NorthWest, geoRect.NorthEast, geoRect.SouthEast, geoRect.SouthWest, geoRect.NorthWest });
            mapLayer.Done();
            LayerSettings.AreaColor = Colors.Transparent;
            LayerSettings.MapLayerViewModel = mapLayer;
            foreach (var dataSet in EnvironmentalDataSets) dataSet.CreateMapLayers();
        }

        public void RemoveMapLayers() { LayerSettings.MapLayerViewModel = null; }
        public void Delete()
        {
            foreach (var scenario in Scenarios.ToList()) scenario.Delete();
            foreach (var dataSet in EnvironmentalDataSets.ToList()) dataSet.Delete();
            Database.Context.Locations.Remove(this);
        }

    }
}
