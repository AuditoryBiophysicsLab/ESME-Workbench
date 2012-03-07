using System.ComponentModel.DataAnnotations;
using HRC.Navigation;

namespace ESME.Database
{
    [ComplexType]
    public class DbGeoRect
    {
        public DbGeoRect(GeoRect geoRect) { _geoRect = new GeoRect(geoRect); }

        public static implicit operator DbGeoRect(GeoRect geoRect) { return new DbGeoRect(geoRect); }
        public static implicit operator GeoRect(DbGeoRect dbGeoRect) { return new GeoRect(dbGeoRect._geoRect); }

        readonly GeoRect _geoRect = new GeoRect();

        public double North
        {
            get { return _geoRect.North; }
            set { _geoRect.North = value; }
        }

        public double South
        {
            get { return _geoRect.South; }
            set { _geoRect.South = value; }
        }

        public double East
        {
            get { return _geoRect.East; }
            set { _geoRect.East = value; }
        }

        public double West
        {
            get { return _geoRect.West; }
            set { _geoRect.West = value; }
        }
    }
}