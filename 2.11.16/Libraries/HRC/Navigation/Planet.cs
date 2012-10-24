using System;

namespace HRC.Navigation
{
    /**
     * Planet datums and parameters. These values are taken from John
     * Snyder's <i>Map Projections --A Working Manual </i> You should add
     * datums as needed, consult the ellips.dat file.
     */
    public class Planet
    {
    // Solar system id's. Add new ones as needed.
    public static int Earth = 3;
    public static int Mars = 4;

    // WGS84 / GRS80 datums
    public static float wgs84_earthPolarRadiusMeters = 6356752.3142f;
    public static double wgs84_earthPolarRadiusMeters_D = 6356752.3142;
    public static float wgs84_earthEquatorialRadiusMeters = 6378137.0f;
    public static double wgs84_earthEquatorialRadiusMeters_D = 6378137.0;
    /* 1 - (minor/major) = 1/298.257 */
    public static float wgs84_earthFlat = 1 - (wgs84_earthPolarRadiusMeters / wgs84_earthEquatorialRadiusMeters);
    /* sqrt(2*f - f^2) = 0.081819221f */
    public static float wgs84_earthEccen = (float) Math.Sqrt(2
            * wgs84_earthFlat - (wgs84_earthFlat * wgs84_earthFlat));
    public static float wgs84_earthEquatorialCircumferenceMeters = (float)MoreMath.TwoPi
            * wgs84_earthEquatorialRadiusMeters;
    public static float wgs84_earthEquatorialCircumferenceKM = wgs84_earthEquatorialCircumferenceMeters / 1000f;
    public static float wgs84_earthEquatorialCircumferenceMiles = wgs84_earthEquatorialCircumferenceKM * 0.62137119f;// HACK
    /* 60.0f * 360.0f -- sixty nm per degree units? */
    public static float wgs84_earthEquatorialCircumferenceNMiles = 21600.0f;
    public static double wgs84_earthEquatorialCircumferenceMeters_D = MoreMath.TwoPi
            * wgs84_earthEquatorialRadiusMeters_D;
    public static double wgs84_earthEquatorialCircumferenceKM_D = wgs84_earthEquatorialCircumferenceMeters_D / 1000;
    public static double wgs84_earthEquatorialCircumferenceMiles_D = wgs84_earthEquatorialCircumferenceKM_D * 0.62137119;// HACK
    /* 60.0f * 360.0f; sixty nm per degree */// units?
    public static double wgs84_earthEquatorialCircumferenceNMiles_D = 21600.0;
    // wgs84_earthEquatorialCircumferenceKM*0.5389892f; // calculated,
    // same as line above.
    // wgs84_earthEquatorialCircumferenceKM*0.5399568f;//HACK use UNIX
    // units? << This was wrong.

    // Mars
    public static float marsEquatorialRadius = 3393400.0f;// meters
    public static float marsEccen = 0.101929f;// eccentricity
    // e
    public static float marsFlat = 0.005208324f;// 1-(1-e^2)^1/2

    // International 1974
    public static float international1974_earthPolarRadiusMeters = 6356911.946f;
    public static float international1974_earthEquatorialRadiusMeters = 6378388f;
    /* 1 - (minor/major) = 1/297 */
    public static float international1974_earthFlat = 1 - (international1974_earthPolarRadiusMeters / international1974_earthEquatorialRadiusMeters);
    /*
     * Extra scale constant for better viewing of maps (do not use
     * this to calculate anything but points to be viewed!) 3384: mattserver/Map.C, 3488: dcw
     */
    public static int defaultPixelsPerMeter = 3272;

    // cannot construct
    private Planet() {}    }
}
