using System.ComponentModel.DataAnnotations;
using System.Data.Entity;
using HRC.Navigation;

namespace ESME.Database
{
    [ComplexType]
    public class DbGeo
    {
        public DbGeo(Geo geo) { _geo = new Geo(geo); }

        public static implicit operator DbGeo(Geo geo) { return new DbGeo(geo); }
        public static implicit operator Geo(DbGeo dbGeo) { return new Geo(dbGeo._geo); }

        public double X { get { return _geo.X; } }
        public double Y { get { return _geo.Y; } }
        public double Z { get { return _geo.Z; } }

        readonly Geo _geo = new Geo();

        public double Latitude
        {
            get { return _geo.Latitude; }
            set { _geo.Latitude = value; }
        }
        public double Longitude
        {
            get { return _geo.Longitude; }
            set { _geo.Longitude = value; }
        }
        internal static void ModelInitialization(DbModelBuilder modelBuilder)
        {
            modelBuilder.ComplexType<DbGeo>()
                .Ignore(p => p.X);
            modelBuilder.ComplexType<DbGeo>()
                .Ignore(p => p.Y);
            modelBuilder.ComplexType<DbGeo>()
                .Ignore(p => p.Z);
        }
    }
}