using HRC.Navigation;

namespace HRC.Navigation
{
    public class Length
    {

        /** Miles, in WGS 84 spherical earth model units. */
        public static Length Miles = new Length("miles", "miles", Planet.wgs84_earthEquatorialCircumferenceMiles_D);
        /** Feet, in WGS 84 spherical earth model units. */
        public static Length Feet = new Length("feet", "ft", Planet.wgs84_earthEquatorialCircumferenceMiles_D*5280.0);
        /** Feet, in WGS 84 spherical earth model units. */
        public static Length Yards = new Length("yards", "yd",Planet.wgs84_earthEquatorialCircumferenceMiles_D*5280.0/3.0);
        /** Meters, in WGS 84 Spherical earth model units. */
        public static Length Meters = new Length("meters", "m", Planet.wgs84_earthEquatorialCircumferenceMeters_D);
        /** Kilometers, in WGS 84 Spherical earth model units. */
        public static Length Kilometers = new Length("kilometers", "km", Planet.wgs84_earthEquatorialCircumferenceKM_D);
        /** Nautical Miles, in WGS 84 Spherical earth model units. */
        public static Length NauticalMiles = new Length("nautical miles", "nm", Planet.wgs84_earthEquatorialCircumferenceNMiles_D);
        /** Decimal Degrees, in WGS 84 Spherical earth model units. */
        public static Length Degrees = new Length("degrees", "deg", 360.0);
        /** Data Mile, in WGS 84 spherical earth model units. */
        public static Length DataMiles = new Length("datamiles", "dm", Planet.wgs84_earthEquatorialCircumferenceMiles_D*5280.0/6000.0);

        /** Units/radian */
        protected double _constant;
        protected string _name;
        protected string _abbreviation;
        protected double _unitEquatorCircumference;

        /**
         * Create a Length, with a name an the number of it's units that go around
         * the earth at its equator. The name and abbreviation are converted to
         * lower case for consistency.
         */

        public Length(string name, string abbreviation, double unitEquatorCircumference)
        {
            _name = name;
            _unitEquatorCircumference = unitEquatorCircumference;
            _constant = unitEquatorCircumference/MoreMath.TwoPi;
            _abbreviation = abbreviation;
        }

        /**
         * Given a number of units provided by this Length, convert to a number of
         * radians.
         */

        public float ToRadians(float numUnits)
        {
            return numUnits/(float) _constant;
        }

        public double ToRadians(double numUnits)
        {
            return numUnits/_constant;
        }

        /**
         * Given a number of radians, convert to the number of units represented by
         * this length.
         */

        public float FromRadians(float numRadians)
        {
            return numRadians*(float) _constant;
        }

        /**
         * Given a number of radians, convert to the number of units represented by
         * this length.
         */

        public double FromRadians(double numRadians)
        {
            return numRadians*_constant;
        }

        public string Name
        {
            get { return _name; }
        }

        public string Abbreviation
        {
            get { return _abbreviation; }
        }

        /**
         * Get a list of the Lengths currently defined as static implementations of
         * this class.
         */

        public static Length[] Lengths
        {
            get
            {
                return new[]
                           {
                               Meters,
                               Kilometers,
                               Feet,
                               Yards,
                               Miles,
                               DataMiles,
                               NauticalMiles,
                               Degrees
                           };
            }
        }
    }
}
