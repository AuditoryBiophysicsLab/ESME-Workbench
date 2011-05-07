using System;
using System.Collections.Generic;
using System.Linq;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class UNESCO
    {
        #region Constants

        const double C00 = 1402.388;
        const double C01 = 5.03830;
        const double C02 = -5.81090E-2;
        const double C03 = 3.3432E-4;
        const double C04 = -1.47797E-6;
        const double C05 = 3.1419E-9;
        const double C10 = 0.153563;
        const double C11 = 6.8999E-4;
        const double C12 = -8.1829E-6;
        const double C13 = 1.3632E-7;
        const double C14 = -6.1260E-10;
        const double C20 = 3.1260E-5;
        const double C21 = -1.7111E-6;
        const double C22 = 2.5986E-8;
        const double C23 = -2.5353E-10;
        const double C24 = 1.0415E-12;
        const double C30 = -9.7729E-9;
        const double C31 = 3.8513E-10;
        const double C32 = -2.3654E-12;
        const double A00 = 1.389;
        const double A01 = -1.262E-2;
        const double A02 = 7.166E-5;
        const double A03 = 2.008E-6;
        const double A04 = -3.21E-8;
        const double A10 = 9.4742E-5;
        const double A11 = -1.2583E-5;
        const double A12 = -6.4928E-8;
        const double A13 = 1.0515E-8;
        const double A14 = -2.0142E-10;
        const double A20 = -3.9064E-7;
        const double A21 = 9.1061E-9;
        const double A22 = -1.6009E-10;
        const double A23 = 7.994E-12;
        const double A30 = 1.100E-10;
        const double A31 = 6.651E-12;
        const double A32 = -3.391E-13;
        const double B00 = -1.922E-2;
        const double B01 = -4.42E-5;
        const double B10 = 7.3637E-5;
        const double B11 = 1.7950E-7;
        const double D00 = 1.727E-3;
        const double D10 = -7.9836E-6;

        #endregion

        #region Sound Speed calculation

        /// <summary>
        ///   Calculates the UNESCO sound speed profile for a given location
        ///   Formula given by Peter Hulton of NUWC ("Hulton, Peter H CIV NUWC NWPT" peter.hulton@navy.mil) in an email on 25 Mar 2009, 09:31 AM to da@bu.edu
        ///   If any of the inputs at any vector location are NaN, the result at that vector location will also be NaN
        /// </summary>
        /// <param name = "location">Latitude and Longitude of the location of the Sound Speed Profile being calculated</param>
        /// <param name = "depthVector">Depth, in meters, at each index of Temperature and Salinity vectors</param>
        /// <param name = "temperatureVector">Temperature, in degrees Celsius, at each depth corresponding to the DepthVector.  NaN indicates no data at a given depth.</param>
        /// <param name = "salinityVector">Salinity, in parts per thousand, at each depth corresponding to the DepthVector.  NaN indicates no data at a given depth.</param>
        /// <returns></returns>
        public static float[] SoundSpeed(EarthCoordinate location, ref float[] depthVector, ref float[] temperatureVector, ref float[] salinityVector)
        {
            if (temperatureVector.Length != salinityVector.Length) throw new ApplicationException("UNESCO.SoundSpeed: Unable to calculate sound speed if temperature and salinity vectors are of unequal length");
            var results = new float[temperatureVector.Length];
#if true
            for (var depth = 0; depth < results.Length; depth++)
            {
                results[depth] = SoundSpeed(location, depthVector[depth], temperatureVector[depth], salinityVector[depth]);
            }
#else
            double[] Pressure_Bar = UNESCO.DepthToPressure_Bar(Location.Latitude_radians, ref DepthVector_Meters);
            double DTP, BTP, ATP, CwTP, cSTP;
            double p, p2, p3;
            double t, t2, t3, t4, t5;
            double s, s32, s2;

            for (int depth = 0; depth < results.Length; depth++)
            {
                p = Pressure_Bar[depth];
                t = TemperatureVector_C[depth];
                s = SalinityVector_ppt[depth];
                if ((double.IsNaN(p)) || (double.IsNaN(t)) || (double.IsNaN(s)))
                {
                    results[depth] = float.NaN;
                    continue;
                }

                p2 = p * p;
                p3 = p2 * p;
                t2 = t * t;
                t3 = t2 * t;
                t4 = t3 * t;
                t5 = t4 * t;
                s2 = s * s;
                s32 = Math.Pow(s, 1.5);
                DTP = D_00 + (D_10 * p);
                BTP = B_00 + (B_01 * t) + ((B_10 + (B_11 * t)) * p);
                ATP = ((A_00 + (A_01 * t) + (A_02 * t2) + (A_03 * t3) + (A_04 * t4))) +
                      ((A_10 + (A_11 * t) + (A_12 * t2) + (A_13 * t3) + (A_14 * t4)) * p) +
                      ((A_20 + (A_21 * t) + (A_22 * t2) + (A_23 * t3)) * p2) +
                      ((A_30 + (A_31 * t) + (A_32 * t2)) * p3);
                CwTP = ((C_00 + (C_01 * t) + (C_02 * t2) + (C_03 * t3) + (C_04 * t4) + (C_05 * t5))) +
                       ((C_10 + (C_11 * t) + (C_12 * t2) + (C_13 * t3) + (C_14 * t4)) * p) +
                       ((C_20 + (C_21 * t) + (C_22 * t2) + (C_23 * t3) + (C_24 * t4)) * p2) +
                       ((C_30 + (C_31 * t) + (C_32 * t2)) * p3);
                cSTP = CwTP + (ATP * s) + (BTP * s32) + (DTP * s2);
                results[depth] = (float)cSTP;
            }
#endif
            return results;
        }

        public static float SoundSpeed(EarthCoordinate location, float depth, float temperature, float salinity)
        {
            var p = DepthToPressure(location.LatitudeRadians, depth);
            if ((double.IsNaN(p)) || (double.IsNaN(temperature)) || (double.IsNaN(salinity))) return float.NaN;

            var p2 = p * p;
            var p3 = p2 * p;
            var t2 = temperature * temperature;
            var t3 = t2 * temperature;
            var t4 = t3 * temperature;
            var t5 = t4 * temperature;
            var s2 = salinity * salinity;
            var s32 = Math.Pow(salinity, 1.5);
            var dtp = D00 + (D10 * p);
            var btp = B00 + (B01 * temperature) + ((B10 + (B11 * temperature)) * p);
            var atp = ((A00 + (A01 * temperature) + (A02 * t2) + (A03 * t3) + (A04 * t4))) + ((A10 + (A11 * temperature) + (A12 * t2) + (A13 * t3) + (A14 * t4)) * p) + ((A20 + (A21 * temperature) + (A22 * t2) + (A23 * t3)) * p2) + ((A30 + (A31 * temperature) + (A32 * t2)) * p3);
            var cwTp = ((C00 + (C01 * temperature) + (C02 * t2) + (C03 * t3) + (C04 * t4) + (C05 * t5))) + ((C10 + (C11 * temperature) + (C12 * t2) + (C13 * t3) + (C14 * t4)) * p) + ((C20 + (C21 * temperature) + (C22 * t2) + (C23 * t3) + (C24 * t4)) * p2) + ((C30 + (C31 * temperature) + (C32 * t2)) * p3);
            var cStp = cwTp + (atp * salinity) + (btp * s32) + (dtp * s2);
            return (float) cStp;
        }

        #endregion

        #region Depth to Pressure calculation

        /// <summary>
        ///   Returns a pressure vector (in Bar) which corresponds to the depth vector supplied
        ///   Formula given by Peter Hulton of NUWC ("Hulton, Peter H CIV NUWC NWPT" peter.hulton@navy.mil) in an email on 25 Mar 2009, 09:31 AM to da@bu.edu
        /// </summary>
        /// <param name = "latitude">Latideu of depth measurements, IN RADIANS</param>
        /// <param name = "depths">An array of depths, in meters, to convert to pressures</param>
        /// <returns>Pressures corresponding to depths, in bar.</returns>
        public static double[] DepthToPressure(double latitude, ref float[] depths)
        {
            var results = new double[depths.Length];

            for (var depth = 0; depth < depths.Length; depth++)
            {
                results[depth] = DepthToPressure(latitude, depths[depth]);
            }
            return results;
        }

        /// <summary>
        ///   Converts a depth in meters to a pressure, in bar
        /// </summary>
        /// <param name = "latitude">Latitude of the depth measurement, IN RADIANS!!!</param>
        /// <param name = "depth">Depth to convert, in meters</param>
        /// <returns>Pressure, in bar</returns>
        public static double DepthToPressure(double latitude, double depth)
        {
            var sinTheta = Math.Sin(latitude);
            var sinSquaredTheta = sinTheta * sinTheta;
            var gTheta = 9.7803 * (1 + 5.3e-3 * sinSquaredTheta);
            var depth2 = depth * depth;
            var depth3 = depth2 * depth;
            var depth4 = depth3 * depth;

            // The following is from http://resource.npl.co.uk/acoustics/techguides/soundseawater/content.html
            var thyh0Z = 1e-2 * depth / (depth + 100) + 6.2e-6 * depth;
            var kZTheta = (gTheta - 2e-5 * depth) / (9.80612 - 2e-5 * depth);
            var hZ45 = 1.00818e-2 * depth + 2.465e-8 * depth2 - 1.25e-13 * depth3 + 2.8e-19 * depth4;
            var hZTheta = hZ45 * kZTheta;
            var pZTheta = hZTheta - thyh0Z;
            return pZTheta * 10; // Multiply by 10 because the formula outputs pressure in MPa, and 1 MPa = 10 bar.
        }

        #endregion

        #region 3-D array handling (Salinity and Temperature field data conversion into sound speed field)

        /// <summary>
        ///   Create a sound speed file from given temperature and salinity files.
        ///   This routine can use a LOT of memory, OutOfMemoryException may occur if the host machine does not have sufficient resources
        /// </summary>
        /// <param name = "temperatureFile">Name of the ESME Environment Binary (.EEB) file containing temperature data.  
        ///   The temperature data MUST be in the first (or only) data layer in the file</param>
        /// <param name = "salinityFile">Name of the ESME Environment Binary (.EEB) file containing salinity data.  
        ///   The salinity data MUST be in the first (or only) data layer in the file</param>
        /// <param name = "soundSpeedFile">Name of the ESME Environment Binary (.EEB) file that will contain the calculated sound speed data.  
        ///   The calculated sound speed data will be in the only data layer in the file</param>
        /// <param name = "bw">If this function is being called by an ImprovedBackgroundWorker thread, pass the object here, otherwise pass null</param>
        public static void CreateSoundSpeedField(string temperatureFile, string salinityFile, string soundSpeedFile, ImprovedBackgroundWorker bw)
        {
            var curProgressRatio = 0f;

            if (bw != null)
            {
                bw.WorkerReportsProgress = true;
                bw.WorkerSupportsCancellation = true;
                bw.TaskName = "Loading salinity data...";
            }
            else Console.WriteLine("Loading salinity data...");
            var salinity = DataFile.Open(salinityFile);
            //salinity.Layers[0].DataArray.LoadAllData();
            if (bw != null) bw.TaskName = "Loading temperature data...";
            else Console.WriteLine("Loading temperature data...");
            var temperature = DataFile.Open(temperatureFile);
            //temperature.Layers[0].DataArray.LoadAllData();
            var sspLayer = new DataLayer("soundspeed", salinity.Layers[0].TimePeriod, salinity.FileName + "+" + temperature.FileName, "Created from Salinity file: " + salinity.FileName + " and Temperature file: " + temperature.FileName + " on " + DateTime.Now, salinity.Layers[0].LatitudeAxis, salinity.Layers[0].LongitudeAxis, salinity.Layers[0].DepthAxis);
            if (bw != null) bw.TaskName = "Initializing sound speed data...";
            else Console.WriteLine("Initializing sound speed data...");
            var soundspeed = DataFile.Create(soundSpeedFile);
            soundspeed.Layers.Add(sspLayer);
            var depths = salinity.Layers[0].DepthAxis.Values;
#if false
            if (bw == null)
                Console.WriteLine("Calculating sound speed data...");
            float ProgressStep = 1f / (float)salinity.Layers[0].LongitudeAxis.Count;
            for (int lon = 0; lon < salinity.Layers[0].LongitudeAxis.Count; lon++)
            {
                for (int lat = 0; lat < salinity.Layers[0].LatitudeAxis.Count; lat++)
                {
                    salData = salinity.Layers[0].DataArray[(uint)lon, (uint)lat];
                    tempData = salinity.Layers[0].DataArray[(uint)lon, (uint)lat];
                    location = new EarthCoordinate(salinity.Layers[0].LatitudeAxis[lat], salinity.Layers[0].LongitudeAxis[lon]);
                    sspData = UNESCO.SoundSpeed(location, ref depths, ref tempData, ref salData);
                    soundspeed.Layers[0].DataArray[(uint)lon, (uint)lat] = sspData;
                }
                CurProgressRatio += ProgressStep;
                if (bw != null)
                    bw.ReportProgress((int)(100f * CurProgressRatio));
                else
                    Console.Write((CurProgressRatio * 100f).ToString("0") + " % complete                          \r");
            }
            if (bw != null)
                bw.TaskName = "Saving soundspeed data...";
            else
                Console.WriteLine("Saving soundspeed data...");
            soundspeed.Save();
            soundspeed.Close();
            salinity = temperature = soundspeed = null;
            sspLayer = null;
            GC.Collect();
#else
            var progressStep = 1f / salinity.Layers[0].LatitudeAxis.Length;
            var temperaturePoint = new DataPoint(temperature.Layers[0]);
            var salinityPoint = new DataPoint(salinity.Layers[0]);
            foreach (var soundSpeedRow in soundspeed.Layers[0].Rows)
            {
                foreach (var soundSpeedPoint in soundSpeedRow.Points)
                {
                    temperaturePoint.RowIndex = salinityPoint.RowIndex = soundSpeedPoint.RowIndex;
                    temperaturePoint.ColumnIndex = salinityPoint.ColumnIndex = soundSpeedPoint.ColumnIndex;
                    var temperatureData = temperaturePoint.Data;
                    var salinityData = salinityPoint.Data;

                    soundSpeedPoint.Data = SoundSpeed(temperaturePoint.EarthCoordinate, ref depths, ref temperatureData, ref salinityData);
                }

                curProgressRatio += progressStep;
                if (bw != null) bw.ReportProgress((int) (100f * curProgressRatio));
                else Console.Write((curProgressRatio * 100f).ToString("0") + " % complete \r");
            }
#endif
            if (bw == null) Console.WriteLine("Completed");
        }

        public static void AverageSoundSpeedFields(List<string> sourceSSFFiles, string destinationSSFFile, string timePeriod, ImprovedBackgroundWorker bw)
        {
            if (bw != null)
            {
                bw.WorkerReportsProgress = true;
                bw.WorkerSupportsCancellation = true;
                bw.TaskName = "Loading metadata from first source sound speed file...";
            }
            else Console.WriteLine("Loading metadata from first source sound speed file...");

            var concatenatedSspFilesString = sourceSSFFiles.Aggregate("", (current, t) => current + ", " + t);

            var sourceSspFile = DataFile.Open(sourceSSFFiles[0]);

            // Across what dimension(s) should I be able to average?  
            // Averaging across time for identical location sets seems the most straightforward 
            // because then the other DataAxes (LatitudeAxis, LongitudeAxis, DepthAxis) would be be identical.

            // public DataLayer(string Name, string TimePeriod, string OriginalFilename, string Metadata,
            // DataAxis LatitudeAxis, DataAxis LongitudeAxis, DataAxis DepthAxis)
            //
            // For the AverageSspLayer, I chose one TimePeriod, the TimePeriod of sourceSspFiles[0].  Appropriate?
            // I set OriginalFilename to the output file DestinationSSFFile.  Appropriate?
            // use "soundspeed"
            var averageSspLayer = new DataLayer("soundspeed", timePeriod, destinationSSFFile, "Average generated from Sound Speed files: " + concatenatedSspFilesString + " on " + DateTime.Now, sourceSspFile.Layers[0].LatitudeAxis, sourceSspFile.Layers[0].LongitudeAxis, sourceSspFile.Layers[0].DepthAxis);

            if (bw != null) bw.TaskName = "Initializing average sound speed data...";
            else Console.WriteLine("Initializing average sound speed data...");

            var averageSoundSpeed = DataFile.Create(destinationSSFFile);
            averageSoundSpeed.Layers.Add(averageSspLayer);

            // Make sure the AverageSoundSpeed DataFile is the same size
            if (bw == null) Console.WriteLine("Calculating average sound speed data...");
            var progressStep = 1f / sourceSspFile.Layers[0].LatitudeAxis.Length;

            for (var sourcefile = 0; sourcefile < sourceSSFFiles.Count; sourcefile++)
            {
                var curProgressRatio = 0f;

                concatenatedSspFilesString = "Processing file " + sourceSSFFiles[sourcefile] + ", " + (sourcefile + 1) + " of " + sourceSSFFiles.Count + " ...";
                Console.WriteLine(concatenatedSspFilesString);

                sourceSspFile = DataFile.Open(sourceSSFFiles[sourcefile]);
                var sourceSspLayer = sourceSspFile.Layers[0];
                for (var depth = 0; depth < sourceSspLayer.DepthAxis.Length; depth++)
                {
                    if (sourceSspLayer.DepthAxis[depth] != averageSoundSpeed.Layers[0].DepthAxis[depth]) throw new ApplicationException("Depth axis mismatch");
                }

                var destPoint = new DataPoint(averageSspLayer);
                foreach (var sourceRow in sourceSspLayer.Rows)
                {
                    foreach (var sourcePoint in sourceRow.Points)
                    {
                        destPoint.RowIndex = sourcePoint.RowIndex;
                        destPoint.ColumnIndex = sourcePoint.ColumnIndex;
                        var destData = destPoint.Data;
                        var sourceData = sourcePoint.Data;

                        for (var depth = 0; depth < sourceData.Length; depth++)
                        {
                            destData[depth] += sourceData[depth];

                            if ((sourcefile == sourceSSFFiles.Count - 1)) destData[depth] /= sourceSSFFiles.Count;
                        }
                        destPoint.Data = destData;
                    }

                    curProgressRatio += progressStep;
                    if (bw != null) bw.ReportProgress((int) (100f * curProgressRatio));
                    else Console.Write((curProgressRatio * 100f).ToString("0") + " % complete \r");
                }
                sourceSspFile.Close();
            }

            averageSoundSpeed.Close();

            if (bw != null) bw.TaskName = "Saving average soundspeed data...";
            else Console.WriteLine("Saving average soundspeed data...");
            averageSoundSpeed.Close();
            if (bw == null) Console.WriteLine("Completed");
        }

        #endregion

        // UNESCO Constants and algorithm taken from http://resource.npl.co.uk/acoustics/techguides/soundseawater/content.html
    }
}