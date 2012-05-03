using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Windows.Data;
using ESME.Database;
using ESME.Mapping;
using ESME.Scenarios;
using HRC.Aspects;

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
        //MapLayerViewModel MapLayer { get; set; }
    }

    public class Location : IHaveGuid//, IHaveLayerSettings
    {
        [Key, Initialize]
        public Guid Guid { get; set; }
        public string Name { get; set; }
        public string Comments { get; set; }
        public DbGeoRect GeoRect { get; set; }
        public string StorageDirectory { get; set; }

        //public virtual LayerSettings LayerSettings { get; set; }

        public virtual ICollection<EnvironmentalDataSet> EnvironmentalDataSets { get; set; }
        //public virtual ICollection<Scenario> Scenarios { get; set; }
        public virtual ICollection<LogEntry> Logs { get; set; }

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


        MapLayerViewModel _mapLayer;
        public void CreateMapLayers()
        {
        }
    }
}
