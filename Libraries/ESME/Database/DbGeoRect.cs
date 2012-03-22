using System.ComponentModel.DataAnnotations;
using HRC.Navigation;

namespace ESME.Database
{
    [ComplexType]
    public class DbGeoRect
    {
        public DbGeoRect(){}
        public DbGeoRect(GeoRect geoRect)
        {
            North = geoRect.North;
            South = geoRect.South;
            East = geoRect.East;
            West = geoRect.West;
        }

        public static implicit operator DbGeoRect(GeoRect geoRect) { return new DbGeoRect(geoRect); }
        public static implicit operator GeoRect(DbGeoRect dbGeoRect) { return new GeoRect(dbGeoRect.North, dbGeoRect.South, dbGeoRect.East, dbGeoRect.West); }

        public double North { get; set; }
        public double South { get; set; }
        public double East { get; set; }
        public double West { get; set; }
    }
}