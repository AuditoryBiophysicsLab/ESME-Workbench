using HRC.Navigation;

namespace ESME.Database
{
    public class DbGeo
    {
        public DbGeo(){}
        public DbGeo(Geo geo) { Latitude = geo.Latitude; Longitude = geo.Longitude; }

        public static implicit operator DbGeo(Geo geo) { return new DbGeo(geo); }
        public static implicit operator Geo(DbGeo dbGeo) { return new Geo(dbGeo.Latitude, dbGeo.Longitude); }
        public double Latitude { get; set; }
        public double Longitude { get; set; }
    }
}