using System;
using HRC.Aspects;
using HRC.ViewModels;

namespace HRC.Navigation
{
    public class UTM : ViewModelBase
    {
        #region WGS84 geoid parameters
        const double A = 6378137;    // Equatorial radius (meters)
        const double B = 6356752.3142;    // Polar radius (meters)
        // const double Flattening = 1 / 298.257223563;
        #endregion

        #region Other constants
        const double K0 = 0.9996; // scale on central meridian
        const double ESquared = 1 - (B / A) * (B / A); // square of eccentricity, for use in expansions
        static readonly double E = Math.Sqrt(E);                                  // eccentricity
        //static readonly double EPrime = E / Math.Sqrt(1 - ESquared);
        const double EPrimeSquared = ESquared / (1 - ESquared);
        //const double N = (A - B) / (A + B);

        const double Drad = Math.PI / 180;
        const int M0 = 0; //M0 is M for some origin latitude other than zero. Not needed for standard UTM
        #endregion

        public UTM(double easting, double northing, int zone)
        {
            Easting = easting;
            Northing = northing;
            Zone = zone;
        }

        #region Explicit conversion from Geo to UTM (var myUTM = (UTM)myGeo;)
        public static explicit operator UTM(Geo geo)
        {
            var phi = geo.LatitudeRadians;
            var lngd = geo.Longitude;
            var utmz = 1 + Math.Floor((geo.Longitude + 180) / 6); 
            var zcm = 3 + (6 * (utmz - 1)) - 180; // zcm is the central meridian of the zone, in degrees
            var n = A / Math.Sqrt(1 - Math.Pow(E * Math.Sin(phi), 2));
            var t = Math.Pow(Math.Tan(phi), 2);
            var c = EPrimeSquared * Math.Pow(Math.Cos(phi), 2);
            var a = (lngd - zcm) * Drad * Math.Cos(phi);
            var m = phi * (1 - ESquared * (1 / 4 + ESquared * (3 / 64 + 5 * ESquared / 256)));
            m = m - Math.Sin(2 * phi) * (ESquared * (3 / 8 + ESquared * (3 / 32 + 45 * ESquared / 1024)));
            m = m + Math.Sin(4 * phi) * (ESquared * ESquared * (15 / 256 + ESquared * 45 / 1024));
            m = m - Math.Sin(6 * phi) * (ESquared * ESquared * ESquared * (35 / 3072));
            m = m * a;  //Arc length along standard meridian
            var x = K0 * n * a * (1 + a * a * ((1 - t + c) / 6 + a * a * (5 - 18 * t + t * t + 72 * c - 58 * ESquared) / 120));//Easting relative to CM
            var y = K0 * (m - M0 + n * Math.Tan(phi) * (a * a * (1 / 2 + a * a * ((5 - t + 9 * c + 4 * c * c) / 24 + a * a * (61 - 58 * t + t * t + 600 * c - 330 * ESquared) / 720))));//Northing from equator
            return new UTM(x + 500000, y + 10000000, (int)utmz);
        }
        #endregion

        #region Explicit conversion from UTM to Geo (var myGeo = (Geo)myUTM;)
        public static explicit operator Geo(UTM utm)
        {
            var x = utm.Easting;
            var y = utm.Northing;
            var e1 = (1 - Math.Sqrt(1 - ESquared)) / (1 + Math.Sqrt(1 - ESquared));//Called e1 in USGS PP 1395 also
            var m = M0 + y / K0;
            var mu = m / (A * (1 - ESquared * (1 / 4 + ESquared * (3 / 64 + 5 * ESquared / 256))));
            var phi1 = mu + e1 * (3 / 2 - 27 * e1 * e1 / 32) * Math.Sin(2 * mu) + e1 * e1 * (21 / 16 - 55 * e1 * e1 / 32) * Math.Sin(4 * mu); //Footprint Latitude
            phi1 = phi1 + e1 * e1 * e1 * (Math.Sin(6 * mu) * 151 / 96 + e1 * Math.Sin(8 * mu) * 1097 / 512);
            var c1 = EPrimeSquared * Math.Pow(Math.Cos(phi1), 2);
            var t1 = Math.Pow(Math.Tan(phi1), 2);
            var n1 = A / Math.Sqrt(1 - Math.Pow(E * Math.Sin(phi1), 2));
            var r1 = n1 * (1 - ESquared) / (1 - Math.Pow(E * Math.Sin(phi1), 2));
            var d = (x - 500000) / (n1 * K0);
            var phi = (d * d) * (1 / 2 - d * d * (5 + 3 * t1 + 10 * c1 - 4 * c1 * c1 - 9 * EPrimeSquared) / 24);
            phi = phi + Math.Pow(d, 6) * (61 + 90 * t1 + 298 * c1 + 45 * t1 * t1 - 252 * EPrimeSquared - 3 * c1 * c1) / 720;
            phi = phi1 - (n1 * Math.Tan(phi1) / r1) * phi;
            var zcm = 3 + (6 * (utm.Zone - 1)) - 180; // zcm is the central meridian of the zone, in degrees
            var lng = d * (1 + d * d * ((-1 - 2 * t1 - c1) / 6 + d * d * (5 - 2 * c1 + 28 * t1 - 3 * c1 * c1 + 8 * EPrimeSquared + 24 * t1 * t1) / 120)) / Math.Cos(phi1);
            var lngd = (zcm + lng) / Drad;
            var latd = phi / Drad;            return new Geo(latd, lngd);
        }
        #endregion

        public double Easting { get; set; }
        public double Northing { get; set; }
        public int Zone { get; set; }
    }
}
