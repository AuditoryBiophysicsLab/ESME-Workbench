using System;
using System.Diagnostics;

namespace HRC.Navigation
{
    public class Length
    {
        /** Miles, in WGS 84 spherical earth model units. */
        public static Length MILE = new Length("mile", "miles", Planet.wgs84_earthEquatorialCircumferenceMiles_D);
        /** Feet, in WGS 84 spherical earth model units. */
        public static Length FEET = new Length("feet", "feet", Planet.wgs84_earthEquatorialCircumferenceMiles_D * 5280.0);
        /** Meters, in WGS 84 Spherical earth model units. */
        public static Length METER = new Length("meter", "m", Planet.wgs84_earthEquatorialCircumferenceMeters_D);
        /** Kilometers, in WGS 84 Spherical earth model units. */
        public static Length KM = new Length("kilometer", "km", Planet.wgs84_earthEquatorialCircumferenceKM_D);
        /** Nautical Miles, in WGS 84 Spherical earth model units. */
        public static Length NM = new Length("nautical mile", "nm", Planet.wgs84_earthEquatorialCircumferenceNMiles_D);
        /** Decimal Degrees, in WGS 84 Spherical earth model units. */
        public static Length DECIMAL_DEGREE = new Length("decimal degree", "deg", 360.0);
        /** Radians, in terms of a spherical earth. */
        public static Length RADIAN = new Length("radian", "rad", Math.PI * 2.0);
        /** Data Mile, in WGS 84 spherical earth model units. */
        public static Length DM = new Length("datamile", "dm", Planet.wgs84_earthEquatorialCircumferenceMiles_D * 5280.0 / 6000.0);
        protected String abbr;

        /** Unit/radians */
        protected double constant;
        protected String name;
        protected double unitEquatorCircumference;

        /**
         * Create a Length, with a name an the number of it's units that
         * go around the earth at its equator. The name and abbreviation
         * are converted to lower case for consistency.
         */

        public Length(String name, String abbr, double unitEquatorCircumference)
        {
            this.name = name.ToLower();
            this.unitEquatorCircumference = unitEquatorCircumference;
            constant = unitEquatorCircumference / (Math.PI * 2.0);
            this.abbr = abbr.ToLower();
        }

        /**
     * Given a number of units provided by this Length, convert to a
     * number of radians.
     */

        public float toRadians(float numUnits)
        {
            return numUnits / (float) constant;
        }

        public double toRadians(double numUnits)
        {
            return numUnits / constant;
        }

        /**
     * Given a number of radians, convert to the number of units
     * represented by this length.
     */

        public float fromRadians(float numRadians)
        {
            return numRadians * (float) constant;
        }

        /**
     * Given a number of radians, convert to the number of units
     * represented by this length.
     */

        public double fromRadians(double numRadians)
        {
            return numRadians * constant;
        }

        /**
     * Return the name for this length type.
     */
        public String toString() { return name; }

        /**
     * Return the abbreviation for this length type.
     */
        public String getAbbr() { return abbr; }

        /**
     * Get a list of the Lengths currently defined as static
     * implementations of this class.
     */

        public static Length[] getAvailable()
        {
            return new[]
                   {
                       METER, KM, FEET, MILE, DM, NM, DECIMAL_DEGREE
                   };
        }

        /**
     * Get the Length object with the given name or abbreviation. If
     * nothing exists with that name, then return null. The lower case
     * version of the name or abbreviation is checked against the
     * available options.
     */

        public static Length get(String name)
        {
            Length[] choices = getAvailable();

            for (int i = 0; i < choices.Length; i++)
            {
                if (name.ToLower() == choices[i].toString() || name.ToLower() == choices[i].getAbbr())
                    return choices[i];
            }
            return null;
        }
    }
}