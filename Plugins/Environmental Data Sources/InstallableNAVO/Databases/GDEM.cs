using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.NetCDF;

namespace InstallableNAVO.Databases
{
    public static class GDEM
    {
        public static string FindSalinityFile(NAVOTimePeriod monthIndex, string gdemDirectory)
        {
            var files = Directory.GetFiles(gdemDirectory, GDEMSalinityFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            files = Directory.GetFiles(gdemDirectory, NUWCSalinityFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            throw new FileNotFoundException(string.Format("Could not find requested salinity file, tried {0} and {1}", GDEMSalinityFileName(monthIndex), NUWCSalinityFileName(monthIndex)));
        }

        public static string FindTemperatureFile(NAVOTimePeriod monthIndex, string gdemDirectory)
        {
            var files = Directory.GetFiles(gdemDirectory, GDEMTemperatureFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            files = Directory.GetFiles(gdemDirectory, NUWCTemperatureFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            throw new FileNotFoundException(string.Format("Could not find requested temperature file, tried {0} and {1}", GDEMTemperatureFileName(monthIndex), NUWCTemperatureFileName(monthIndex)));
        }

        static string GDEMTemperatureFileName(NAVOTimePeriod monthIndex) { return "t" + BaseGDEMFileName(monthIndex); }
        static string GDEMSalinityFileName(NAVOTimePeriod monthIndex) { return "s" + BaseGDEMFileName(monthIndex); }
        static string BaseGDEMFileName(NAVOTimePeriod monthIndex)
        {
            if ((int)monthIndex < 1 || (int)monthIndex > 12) throw new ArgumentOutOfRangeException("monthIndex", "must be between 1 and 12, inclusive");
            return "gdemv3s" + string.Format("{0:00}", (int)monthIndex) + ".nc";
        }
        static string NUWCTemperatureFileName(NAVOTimePeriod monthIndex) { return ShortMonthNames[(int)monthIndex] + "_t.nc"; }
        static string NUWCSalinityFileName(NAVOTimePeriod monthIndex) { return ShortMonthNames[(int)monthIndex] + "_s.nc"; }
        static readonly string[] ShortMonthNames = new[] { "noneuary", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec" };

        /// <summary>
        /// Returns true if the directory exists and contains some combination of valid GDEM and/or NUWC file names
        /// </summary>
        /// <param name="gdemDirectory"></param>
        /// <returns></returns>
        public static bool IsDirectoryValid(string gdemDirectory)
        {
            if (!Directory.Exists(gdemDirectory)) return false;
            try
            {
                for (var month = (int)NAVOTimePeriod.January; month <= (int)NAVOTimePeriod.December; month++)
                {
                    FindTemperatureFile((NAVOTimePeriod)month, gdemDirectory);
                    FindSalinityFile((NAVOTimePeriod)month, gdemDirectory);
                }
                return true;
            }
            catch (FileFormatException)
            {
                return false;
            }
        }

        public static SoundSpeedField ReadFile(string fileName, string dataVarName, NAVOTimePeriod month, GeoRect region)
        {
            var myFile = NetCDFFile.Open(fileName);
            Logger.Log("in ReadFile: 0.1");
            var lats = ((NcVarDouble)myFile.Variables.Single(var => var.Name == "lat")).ToArray();
            Logger.Log("in ReadFile: 0.2");
            var lons = ((NcVarDouble)myFile.Variables.Single(var => var.Name == "lon")).ToArray();
            Logger.Log("in ReadFile: 0.3");
            var depths = ((NcVarDouble)myFile.Variables.Single(var => var.Name == "depth")).ToArray();
            Logger.Log("in ReadFile: 0.4");
            var data = (NcVarShort)myFile.Variables.Single(var => var.Name == dataVarName);
            data.Filename = fileName;
            Logger.Log("in ReadFile: 0.5");
            var missingValue = ((NcAttShort)data.Attributes.Single(att => att.Name == "missing_value"))[0];
            Logger.Log("in ReadFile: 0.6");
            var scaleFactor = ((NcAttFloat)data.Attributes.Single(att => att.Name == "scale_factor"))[0];
            Logger.Log("in ReadFile: 0.7");
            var addOffset = ((NcAttFloat)data.Attributes.Single(att => att.Name == "add_offset"))[0];
            Logger.Log("in ReadFile: 0.8");
            
            var north = region.North;
            var south = region.South;
            var east = region.East;
            var west = region.West;
            
            if (lons.First() > west) west += 360;
            if (lons.Last() < west) west -= 360;
            if (lons.First() > east) east += 360;
            if (lons.Last() < east) east -= 360;

            var lonMap = new List<AxisMap>();
            var latMap = new List<AxisMap>();
            for (var i = 0; i < lons.Length; i++) if ((lons[i] >= west) && (lons[i] <= east)) lonMap.Add(new AxisMap((float)lons[i], i));
            for (var i = 0; i < lats.Length; i++) if (lats[i] >= south && lats[i] <= north) latMap.Add(new AxisMap((float)lats[i], i));
            var selectedLons = lonMap.Select(x => x.Value).ToArray();
            var selectedLats = latMap.Select(y => y.Value).ToArray();

            var latCount = selectedLats.Length;
            var lonCount = selectedLons.Length;

            var newFieldEnvironmentData = new List<SoundSpeedProfile>();
            Logger.Log("in ReadFile: 1");

            for (var lonIndex = 0; lonIndex < lonCount; lonIndex++)
            {
                var lon = lonMap[lonIndex].Value;
                var wrappedLon = lon;
                while (wrappedLon > 180) wrappedLon -= 360;
                while (wrappedLon < -180) wrappedLon += 360;
                Logger.Log("in ReadFile: 2");

                var lonSourceIndex = lonMap[lonIndex].Index;
                for (var latIndex = 0; latIndex < latCount; latIndex++)
                {
                    Logger.Log("in ReadFile: 3");
                    var lat = latMap[latIndex].Value;
                    var latSourceIndex = latMap[latIndex].Index;
                    var newProfile = new SoundSpeedProfile(new EarthCoordinate(lat, wrappedLon));
                    for (var depthIndex = 0; depthIndex < depths.Length; depthIndex++)
                    {
                        Logger.Log("in ReadFile: 4");
                        Logger.Log("in ReadFile: 5");
                        var curValue = data[(uint)depthIndex, (uint)latSourceIndex, (uint)lonSourceIndex];
                        Logger.Log("in ReadFile: 6");
                        if (curValue == missingValue) break;
                        newProfile.Data.Add(new DepthValuePair<float>((float)depths[depthIndex], ((curValue) * scaleFactor) + addOffset));
                    }
                    if (newProfile.Data.Count > 0) newFieldEnvironmentData.Add(newProfile);
                }
            }
            var newField = new SoundSpeedField { TimePeriod = month };
            newField.EnvironmentData.AddRange(newFieldEnvironmentData);
            newField.EnvironmentData.Sort();
            return newField;
        }

        private class AxisMap
        {
            public float Value { get; private set; }
            public int Index { get; private set; }

            public AxisMap(float value, int index)
            {
                Value = value;
                Index = index;
            }

            public override string ToString() { return string.Format("({0}, {1})", Value, Index); }
        }
    }
}
