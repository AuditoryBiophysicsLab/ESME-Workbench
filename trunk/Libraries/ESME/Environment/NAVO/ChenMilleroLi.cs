using System;
using HRC.Navigation;

namespace ESME.Environment.NAVO
{
    internal class ChenMilleroLi
    {
        #region Constants

        const double LI11 = 0.0029;
        const double LI12 = -2.19E-4;
        const double LI13 = 1.4E-5;
        const double LI21 = -4.76E-6;
        const double LI22 = 3.47E-7;
        const double LI23 = -2.59E-8;
        const double LI31 = 2.68E-9;

        const double A00 = 1.389;
        const double A01 = -1.262E-2;
        const double A02 = 7.164E-5;
        const double A03 = 2.006E-6;
        const double A04 = -3.21E-8;
        const double A10 = 9.4742E-5;
        const double A11 = -1.2580E-5;
        const double A12 = -6.4885E-8;
        const double A13 = 1.0507E-8;
        const double A14 = -2.0122E-10;
        const double A20 = -3.9064E-7;
        const double A21 = 9.1041E-9;
        const double A22 = -1.6002E-10;
        const double A23 = 7.988E-12;
        const double A30 = 1.100E-10;
        const double A31 = 6.649E-12;
        const double A32 = -3.389E-13;

        const double B00 = -1.922E-2;
        const double B01 = -4.42E-5;
        const double B10 = 7.3637E-5;
        const double B11 = 1.7945E-7;

        const double C00 = 1402.388;
        const double C01 = 5.03711;
        const double C02 = -5.80852E-2;
        const double C03 = 3.3420E-4;
        const double C04 = -1.47800E-6;
        const double C05 = 3.1464E-9;
        const double C10 = 0.153563;
        const double C11 = 6.8982E-4;
        const double C12 = -8.1788E-6;
        const double C13 = 1.3621E-7;
        const double C14 = -6.1185E-10;
        const double C20 = 3.1260E-5;
        const double C21 = -1.7107E-6;
        const double C22 = 2.5974E-8;
        const double C23 = -2.5335E-10;
        const double C24 = 1.0405E-12;
        const double C30 = -9.7729E-9;
        const double C31 = 3.8504E-10;
        const double C32 = -2.3643E-12;

        const double D00 = 1.727E-3;
        const double D10 = -7.9836E-6;

        #endregion

        #region Sound Speed calculation

        /// <summary>
        ///   Calculates the OAML Navy sound speed profile for a given location
        ///   Forumula ported from ChenMilleroLiAlgorithm.java by Dave Anderson on 22 Feb 2010
        ///   Original Java source provided by Bill Sutphin of NUWC on 2 Dec 2010
        ///   If any of the inputs at any vector location are NaN, the result at that vector location will also be NaN
        /// </summary>
        /// <param name = "location">Latitude and Longitude of the location of the Sound Speed Profile being calculated</param>
        /// <param name = "depthVector">Depth, in meters, at each index of Temperature and Salinity vectors</param>
        /// <param name = "temperatureVector">Temperature, in degrees Celsius, at each depth corresponding to the DepthVector.  NaN indicates no data at a given depth.</param>
        /// <param name = "salinityVector">Salinity, in parts per thousand, at each depth corresponding to the DepthVector.  NaN indicates no data at a given depth.</param>
        /// <returns></returns>
        public static float[] SoundSpeed(EarthCoordinate location, ref float[] depthVector, ref float[] temperatureVector, ref float[] salinityVector)
        {
            if (temperatureVector.Length != salinityVector.Length) throw new ApplicationException("ChenMilleroLi.SoundSpeed: Unable to calculate sound speed if temperature and salinity vectors are of unequal length");
            var results = new float[temperatureVector.Length];
            for (var depth = 0; depth < results.Length; depth++)
                results[depth] = SoundSpeed(location, depthVector[depth], temperatureVector[depth], salinityVector[depth]);
            return results;
        }

        public static SoundSpeedProfile SoundSpeed(SoundSpeedProfile temperatureProfile, SoundSpeedProfile salinityProfile)
        {
            if (temperatureProfile.Data.Count != salinityProfile.Data.Count) throw new ApplicationException("ChenMilleroLi.SoundSpeed: Unable to calculate sound speed if temperature and salinity profiles are of unequal length");
            var results = new SoundSpeedProfile(temperatureProfile);
            for (var index = 0; index < temperatureProfile.Data.Count; index++)
            {
                var temperature = temperatureProfile.Data[index];
                var salinity = salinityProfile.Data[index];
                if (temperature.Depth != salinity.Depth) throw new ApplicationException("ChenMilleroLi.SoundSpeed: Unable to calculate sound speed if temperature and salinity depths do not match");
                results.Data.Add(new DepthValuePair<float>{Depth = temperature.Depth, Value = SoundSpeed(temperatureProfile, temperature.Depth, temperature.Value, salinity.Value)});
            }
            return results;
        }

        public static float SoundSpeed(EarthCoordinate location, float depth, float temperature, float salinity)
        {
            var tempSq = temperature * temperature;
            var tempCu = tempSq * temperature;
            var tempQuad = tempCu * temperature;
            var tempQuin = tempQuad * temperature;

            var pressure = DepthToPressure(location.LatitudeRadians, depth);
            var pressSq = pressure * pressure;
            var pressCu = pressSq * pressure;

            var result = (( 
                          // computes sound speed of pure water
                          ((C00 + C01 * temperature + C02 * tempSq + C03 * tempCu + C04 * tempQuad + C05 * tempQuin)) + ((C10 + C11 * temperature + C12 * tempSq + C13 * tempCu + C14 * tempQuad) * pressure) + ((C20 + C21 * temperature + C22 * tempSq + C23 * tempCu + C24 * tempQuad) * pressSq) + ((C30 + C31 * temperature + C32 * tempSq) * pressCu)) -
                          // compute correction
                          (((LI11 + LI12 * temperature + LI13 * tempSq) * pressure) + ((LI21 + LI22 * temperature + LI23 * tempSq) * pressSq) + ((LI31) * pressCu))) +
                          // compute a
                         (((A00 + A01 * temperature + A02 * tempSq + A03 * tempCu + A04 * tempQuad)) + ((A10 + A11 * temperature + A12 * tempSq + A13 * tempCu + A14 * tempQuad) * pressure) + ((A20 + A21 * temperature + A22 * tempSq + A23 * tempCu) * pressSq) + ((A30 + A31 * temperature + A32 * tempSq) * pressCu)) * salinity +
                         // compute b
                         (((B00 + B01 * temperature)) + ((B10 + B11 * temperature) * pressure)) * Math.Pow(salinity, 3.0 / 2.0) +
                         // compute d
                         (((D00 + D10 * pressure))) * salinity * salinity;

            return (float) result;
        }

        #endregion

        #region Depth to Pressure calculation

        /// <summary>
        ///   Converts a depth in meters to a pressure, in decibar
        ///   Forumula ported from SoundSpeedAlgorithmBase.java by Dave Anderson on 22 Feb 2010
        ///   Original Java source provided by Bill Sutphin of NUWC on 2 Dec 2010
        /// </summary>
        /// <param name = "latitude">Latideu of depth measurements, IN RADIANS</param>
        /// <param name = "depths">An array of depths, in meters, to convert to pressures</param>
        /// <returns>Pressures corresponding to depths, in bar.</returns>
        public static double[] DepthToPressure(double latitude, ref float[] depths)
        {
            var results = new double[depths.Length];

            for (var depth = 0; depth < depths.Length; depth++)
                results[depth] = DepthToPressure(latitude, depths[depth]);
            return results;
        }

        /// <summary>
        ///   Converts a depth in meters to a pressure, in bar
        ///   Converted from SoundSpeedAlgorithmBase.java by Dave Anderson on 22 Feb 2011
        ///   Original source code provided by NUWC MSMT/Bill Sutphin
        ///   see OAML-SDD-STD-81_D2PA_1.0_Jul_2002_(U).doc
        /// </summary>
        /// <param name = "latitude">Latitude of the depth measurement, IN RADIANS!!!</param>
        /// <param name = "depth">Depth to convert, in meters</param>
        /// <returns>Pressure, in bar</returns>
        public static double DepthToPressure(double latitude, double depth)
        {
            const double a1 = 9.72569;
            const double a2 = -2.2512e-5;
            const double a3 = 2.279e-10;
            const double a4 = -1.82e-15;

            const double b = 0.5 * 2.184e-6;

            const double c = 9.780318;
            const double c2 = 5.2788e-3;
            const double c4 = 2.36e-5;

            const double deltaP = 0.0002;

            var p = depth;

            var sinLat = Math.Sin(latitude);
            var sinSquaredLat = sinLat * sinLat;

            var g = c * (1.0 + (c2 * sinSquaredLat) + (c4 * sinSquaredLat * sinSquaredLat));

            for (var i = 0; i < 10000; i++)
            {
                var temz = a1 * p + a2 * (p * p) + a3 * (p * p * p) + a4 * (p * p * p * p);
                temz = temz / (g + b * p);
                if ((Math.Abs(depth - temz)) > deltaP) p = p + ((depth - temz) * 0.5);
                else break;
            }

            return p / 10.0;
        }

        #endregion
    }
}