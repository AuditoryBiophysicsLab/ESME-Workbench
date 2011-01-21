using System;
using System.Collections.Generic;
using System.Text;
using HRC.Navigation;
using HRC.Utility;

namespace ESME.Environment
{
    public class UNESCO
    {
        // UNESCO Constants and algorithm taken from http://resource.npl.co.uk/acoustics/techguides/soundseawater/content.html
        #region Constants
        private const double C_00 = 1402.388;
        private const double C_01 = 5.03830;
        private const double C_02 = -5.81090E-2;
        private const double C_03 = 3.3432E-4;
        private const double C_04 = -1.47797E-6;
        private const double C_05 = 3.1419E-9;
        private const double C_10 = 0.153563;
        private const double C_11 = 6.8999E-4;
        private const double C_12 = -8.1829E-6;
        private const double C_13 = 1.3632E-7;
        private const double C_14 = -6.1260E-10;
        private const double C_20 = 3.1260E-5;
        private const double C_21 = -1.7111E-6;
        private const double C_22 = 2.5986E-8;
        private const double C_23 = -2.5353E-10;
        private const double C_24 = 1.0415E-12;
        private const double C_30 = -9.7729E-9;
        private const double C_31 = 3.8513E-10;
        private const double C_32 = -2.3654E-12;
        private const double A_00 = 1.389;
        private const double A_01 = -1.262E-2;
        private const double A_02 = 7.166E-5;
        private const double A_03 = 2.008E-6;
        private const double A_04 = -3.21E-8;
        private const double A_10 = 9.4742E-5;
        private const double A_11 = -1.2583E-5;
        private const double A_12 = -6.4928E-8;
        private const double A_13 = 1.0515E-8;
        private const double A_14 = -2.0142E-10;
        private const double A_20 = -3.9064E-7;
        private const double A_21 = 9.1061E-9;
        private const double A_22 = -1.6009E-10;
        private const double A_23 = 7.994E-12;
        private const double A_30 = 1.100E-10;
        private const double A_31 = 6.651E-12;
        private const double A_32 = -3.391E-13;
        private const double B_00 = -1.922E-2;
        private const double B_01 = -4.42E-5;
        private const double B_10 = 7.3637E-5;
        private const double B_11 = 1.7950E-7;
        private const double D_00 = 1.727E-3;
        private const double D_10 = -7.9836E-6;
        #endregion

        #region Sound Speed calculation
        /// <summary>
        /// Calculates the UNESCO sound speed profile for a given location
        /// Formula given by Peter Hulton of NUWC ("Hulton, Peter H CIV NUWC NWPT" peter.hulton@navy.mil) in an email on 25 Mar 2009, 09:31 AM to da@bu.edu
        /// If any of the inputs at any vector location are NaN, the result at that vector location will also be NaN
        /// </summary>
        /// <param name="Location">Latitude and Longitude of the location of the Sound Speed Profile being calculated</param>
        /// <param name="DepthVector_Meters">Depth, in meters, at each index of Temperature and Salinity vectors</param>
        /// <param name="TemperatureVector_C">Temperature, in degrees Celsius, at each depth corresponding to the DepthVector.  NaN indicates no data at a given depth.</param>
        /// <param name="SalinityVector_ppt">Salinity, in parts per thousand, at each depth corresponding to the DepthVector.  NaN indicates no data at a given depth.</param>
        /// <returns></returns>
        public static float[] SoundSpeed(EarthCoordinate Location, ref float[] DepthVector_Meters, ref float[] TemperatureVector_C, ref float[] SalinityVector_ppt)
        {
            if (TemperatureVector_C.Length != SalinityVector_ppt.Length) throw new ApplicationException("UNESCO.SoundSpeed: Unable to calculate sound speed if temperature and salinity vectors are of unequal length");
            float[] results = new float[TemperatureVector_C.Length];
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
            return results;
        }
        #endregion

        #region Depth to Pressure calculation
        /// <summary>
        /// Returns a pressure vector (in Bar) which corresponds to the depth vector supplied
        /// Formula given by Peter Hulton of NUWC ("Hulton, Peter H CIV NUWC NWPT" peter.hulton@navy.mil) in an email on 25 Mar 2009, 09:31 AM to da@bu.edu
        /// </summary>
        /// <param name="DepthVector_Meters">An array of depths, in meters, to convert to pressures</param>
        /// <returns></returns>
        public static double[] DepthToPressure_Bar(double Latitude_Radians, ref float[] DepthVector_Meters)
        {
            // Stuff for Hulton algorithm
            double[] results = new double[DepthVector_Meters.Length];
            double sinTheta = Math.Sin(Latitude_Radians);
            double sinSquaredTheta = sinTheta * sinTheta;
#if false
            double c1 = (5.92 + (5.25 * sinTheta * sinTheta)) * 1e-3;
            double c2 = 1.0 - c1;
            double c3 = Math.Pow(8.84, -6.0);
            const double c4 = 4.42e-6;
            double p;
#endif
            // The following is from http://resource.npl.co.uk/acoustics/techguides/soundseawater/content.html
            double thyh0Z, curDepth, k_Z_Theta, h_Z_45, h_Z_Theta, P_Z_Theta;
            double gTheta = 9.7803 * (1 + 5.3e-3 * sinSquaredTheta);
 
            for (int depth = 0; depth < DepthVector_Meters.Length; depth++)
            {
                curDepth = DepthVector_Meters[depth];
#if false
                // Stuff for Hulton algorithm
                p = ((1.0 - c1) - Math.Sqrt((c2 * c2) - c3 * curDepth)) / c4;
                results[depth] = p / 10;
#else
                // The following is from http://resource.npl.co.uk/acoustics/techguides/soundseawater/content.html
                thyh0Z = 1e-2 * curDepth / (curDepth + 100) + 6.2e-6 * curDepth;
                k_Z_Theta = (gTheta - 2e-5 * curDepth) / (9.80612 - 2e-5 * curDepth);
                h_Z_45 = 1.00818e-2 * curDepth + 2.465e-8 * curDepth * curDepth - 1.25e-13 * curDepth * curDepth * curDepth + 2.8e-19 * curDepth * curDepth * curDepth * curDepth;
                h_Z_Theta = h_Z_45 * k_Z_Theta;
                P_Z_Theta = h_Z_Theta - thyh0Z;
                results[depth] = P_Z_Theta * 10; // Multiply by 10 because the formula outputs pressure in MPa, and 1 MPa = 10 bar.
#endif
            }
            return results;
        }
        #endregion

        #region 3-D array handling (Salinity and Temperature field data conversion into sound speed field)
        /// <summary>
        /// Create a sound speed file from given temperature and salinity files.
        /// This routine can use a LOT of memory, OutOfMemoryException may occur if the host machine does not have sufficient resources
        /// </summary>
        /// <param name="TemperatureFile">Name of the ESME Environment Binary (.EEB) file containing temperature data.  
        /// The temperature data MUST be in the first (or only) data layer in the file</param>
        /// <param name="SalinityFile">Name of the ESME Environment Binary (.EEB) file containing salinity data.  
        /// The salinity data MUST be in the first (or only) data layer in the file</param>
        /// <param name="SoundSpeedFile">Name of the ESME Environment Binary (.EEB) file that will contain the calculated sound speed data.  
        /// The calculated sound speed data will be in the only data layer in the file</param>
        /// <param name="bw">If this function is being called by an ImprovedBackgroundWorker thread, pass the object here, otherwise pass null</param>
        public static void CreateSoundSpeedField(string TemperatureFile, string SalinityFile, string SoundSpeedFile, ImprovedBackgroundWorker bw)
        {
            DataFile salinity, temperature, soundspeed;
            DataLayer sspLayer;
            float[] depths;
            float CurProgressRatio = 0f;

            if (bw != null)
            {
                bw.WorkerReportsProgress = true;
                bw.WorkerSupportsCancellation = true;
                bw.TaskName = "Loading salinity data...";
            }
            else
                Console.WriteLine("Loading salinity data...");
            salinity = DataFile.Open(SalinityFile);
            //salinity.Layers[0].DataArray.LoadAllData();
            if (bw != null)
                bw.TaskName = "Loading temperature data...";
            else
                Console.WriteLine("Loading temperature data...");
            temperature = DataFile.Open(TemperatureFile);
            //temperature.Layers[0].DataArray.LoadAllData();
            sspLayer = new DataLayer("soundspeed", salinity.Layers[0].TimePeriod, salinity.FileName + "+" + temperature.FileName,
                "Created from Salinity file: " + salinity.FileName + " and Temperature file: " + temperature.FileName + " on " + DateTime.Now.ToString(),
                salinity.Layers[0].LatitudeAxis, salinity.Layers[0].LongitudeAxis, salinity.Layers[0].DepthAxis);
            if (bw != null)
                bw.TaskName = "Initializing sound speed data...";
            else
                Console.WriteLine("Initializing sound speed data...");
            soundspeed = DataFile.Create(SoundSpeedFile);
            soundspeed.Layers.Add(sspLayer);
            depths = salinity.Layers[0].DepthAxis.Values;
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
            float ProgressStep = 1f / (float)salinity.Layers[0].LatitudeAxis.Length;
            DataPoint temperaturePoint, salinityPoint;
            float[] temperatureData, salinityData;
            temperaturePoint = new DataPoint(temperature.Layers[0]);
            salinityPoint = new DataPoint(salinity.Layers[0]);
            foreach (DataRow soundSpeedRow in soundspeed.Layers[0].Rows)
            {
                foreach (DataPoint soundSpeedPoint in soundSpeedRow.Points)
                {
                    temperaturePoint.RowIndex = salinityPoint.RowIndex = soundSpeedPoint.RowIndex;
                    temperaturePoint.ColumnIndex = salinityPoint.ColumnIndex = soundSpeedPoint.ColumnIndex;
                    temperatureData = temperaturePoint.Data;
                    salinityData = salinityPoint.Data;

                    soundSpeedPoint.Data = UNESCO.SoundSpeed(temperaturePoint.EarthCoordinate, ref depths, ref temperatureData, ref salinityData);
                }

                CurProgressRatio += ProgressStep;
                if (bw != null)
                    bw.ReportProgress((int)(100f * CurProgressRatio));
                else
                    Console.Write((CurProgressRatio * 100f).ToString("0") + " % complete \r");
            }
#endif
            if (bw == null)
                Console.WriteLine("Completed");
        }

        public static void AverageSoundSpeedFields(List<string> SourceSSFFiles, string DestinationSSFFile, string TimePeriod, ImprovedBackgroundWorker bw)
        {
            DataFile sourceSspFile, AverageSoundSpeed;
            DataLayer sourceSspLayer, AverageSspLayer;
           
            float CurProgressRatio = 0f;
            string ConcatenatedSspFilesString = "";

            if (bw != null)
            {
                bw.WorkerReportsProgress = true;
                bw.WorkerSupportsCancellation = true;
                bw.TaskName = "Loading metadata from first source sound speed file...";
            }
            else
                Console.WriteLine("Loading metadata from first source sound speed file...");

            for (int i = 0; i < SourceSSFFiles.Count; i++)
            {
                ConcatenatedSspFilesString = ConcatenatedSspFilesString + ", " + SourceSSFFiles[i];  // ? Path.GetFilenAME ?
            }

            sourceSspFile = DataFile.Open(SourceSSFFiles[0]);

            // Across what dimension(s) should I be able to average?  
            // Averaging across time for identical location sets seems the most straightforward 
            // because then the other DataAxes (LatitudeAxis, LongitudeAxis, DepthAxis) would be be identical.

            // public DataLayer(string Name, string TimePeriod, string OriginalFilename, string Metadata,
            // DataAxis LatitudeAxis, DataAxis LongitudeAxis, DataAxis DepthAxis)
            //
            // For the AverageSspLayer, I chose one TimePeriod, the TimePeriod of sourceSspFiles[0].  Appropriate?
            // I set OriginalFilename to the output file DestinationSSFFile.  Appropriate?
            // use "soundspeed"
            AverageSspLayer = new DataLayer("soundspeed", TimePeriod, DestinationSSFFile,
            "Average generated from Sound Speed files: " + ConcatenatedSspFilesString + " on " + DateTime.Now.ToString(),
            sourceSspFile.Layers[0].LatitudeAxis, sourceSspFile.Layers[0].LongitudeAxis, sourceSspFile.Layers[0].DepthAxis);

            if (bw != null)
                bw.TaskName = "Initializing average sound speed data...";
            else
                Console.WriteLine("Initializing average sound speed data...");

            AverageSoundSpeed = DataFile.Create(DestinationSSFFile); // Data Layers in an existing file will be cleared
            AverageSoundSpeed.Layers.Add(AverageSspLayer);

            // Make sure the AverageSoundSpeed DataFile is the same size
            if (bw == null)
                Console.WriteLine("Calculating average sound speed data...");
            float ProgressStep = 1f / (float)sourceSspFile.Layers[0].LatitudeAxis.Length;

            for (int sourcefile = 0; sourcefile < SourceSSFFiles.Count; sourcefile++)
            {
                CurProgressRatio = 0f;

                ConcatenatedSspFilesString = "Processing file " + SourceSSFFiles[sourcefile] + ", " + (sourcefile + 1) + " of " + SourceSSFFiles.Count + " ...";
                Console.WriteLine(ConcatenatedSspFilesString);

                sourceSspFile = DataFile.Open(SourceSSFFiles[sourcefile]);
                sourceSspLayer =  sourceSspFile.Layers[0];
                for (int depth = 0; depth < sourceSspLayer.DepthAxis.Length; depth++)
                {
                    if (sourceSspLayer.DepthAxis[depth] != AverageSoundSpeed.Layers[0].DepthAxis[depth])
                        throw new ApplicationException("Depth axis mismatch");
                }

                var destPoint = new DataPoint(AverageSspLayer);
                foreach (DataRow sourceRow in sourceSspLayer.Rows)
                {
                    foreach (DataPoint sourcePoint in sourceRow.Points)
                    {
                        destPoint.RowIndex = sourcePoint.RowIndex;
                        destPoint.ColumnIndex = sourcePoint.ColumnIndex;
                        var destData = destPoint.Data;
                        var sourceData = sourcePoint.Data;

                        for (int depth = 0; depth < sourceData.Length; depth++)
                        {
                            destData[depth] += sourceData[depth];

                            if ((sourcefile == SourceSSFFiles.Count - 1))
                                destData[depth] /= SourceSSFFiles.Count;
                        }
                        destPoint.Data = destData;
                    }

                    CurProgressRatio += ProgressStep;
                    if (bw != null)
                        bw.ReportProgress((int)(100f * CurProgressRatio));
                    else
                        Console.Write((CurProgressRatio * 100f).ToString("0") + " % complete \r");
                }
                sourceSspFile.Close();
            }

            AverageSoundSpeed.Close();

            if (bw != null)
                bw.TaskName = "Saving average soundspeed data...";
            else
                Console.WriteLine("Saving average soundspeed data...");
            AverageSoundSpeed.Close();
            AverageSoundSpeed = null;
            AverageSspLayer = null;
            if (bw == null)
                Console.WriteLine("Completed");

        }
        #endregion
    }
}
