using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using HRC.Navigation;
using HRC.NetCDF;

namespace NAVO
{
    public static class GDEM
    {
        [Flags]
        public enum GDEMFlags
        {
            None = 0x0,
            Temperature = 0x01,
            Salinity = 0x02,
        }

#if false
        public static Task ImportByMonthAsync(string outputPath, bool temperature, bool salinity, ICollection<NAVOTimePeriod> months, GeoRect geoRect, IProgress<string> currentState = null, IProgress<float> progress = null)
        {
            var multiplier = 0;
            if (temperature) multiplier++;
            if (salinity) multiplier++;
            if (multiplier == 0) throw new ApplicationException("one or both of temperature and salinity must be true");
            var progressStep = 100f / (months.Count * multiplier);
            var totalProgress = 0f;
            var actionBlock = new ActionBlock<NAVOTimePeriod>(month =>
                {
                    var monthName = month.ToString().ToLower();
                    var speed = new SoundSpeed();
                    if (temperature)
                    {
                        if (currentState != null) lock (currentState) currentState.Report(string.Format("Importing {0} temperature", monthName));
                        if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                        speed.SoundSpeedFields.Add(ReadFile(FindTemperatureFile(month), "water_temp", month, geoRect));
                        speed.Save(Path.Combine(outputPath, string.Format("{0}.{1}", monthName, "temperature")));
                    }
                    if (salinity)
                    {
                        speed = new SoundSpeed();
                        if (currentState != null) lock (currentState) currentState.Report(string.Format("Importing {0} salinity", monthName));
                        if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                        speed.SoundSpeedFields.Add(ReadFile(FindSalinityFile(month), "salinity", month, geoRect));
                        speed.Save(Path.Combine(outputPath, string.Format("{0}.{1}", monthName, "salinity")));
                    }
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = 1,
                });
            foreach (var month in months) actionBlock.Post(month);
            actionBlock.Complete();
            return actionBlock.Completion;
        }

        public async static Task<List<SoundSpeedField>> CalculateSoundSpeedFieldsAsync(List<SoundSpeedField> temperatureFields, List<SoundSpeedField> salinityFields, Bathymetry bathymetry = null, IProgress<string> currentState = null, IProgress<float> progress = null)
        {
            var temperaturePeriods = temperatureFields.Select(field => field.TimePeriod).ToList();
            var salinityPeriods = salinityFields.Select(field => field.TimePeriod).ToList();
            temperaturePeriods.Sort();
            salinityPeriods.Sort();
            if ((temperaturePeriods.Count != salinityPeriods.Count) || (temperaturePeriods.Where((t, i) => t != salinityPeriods[i]).Any()))
                throw new ArgumentException("temperature and salinity contain data for different time periods");
            var progressStep = 100f / temperaturePeriods.Count;
            var totalProgress = 0f;
            var transformBlock = new TransformBlock<NAVOTimePeriod, SoundSpeedField>(month =>
                {
                    var monthName = month.ToString().ToLower();
                    if (currentState != null) lock (currentState) currentState.Report(string.Format("Calculating {0} soundspeed", monthName));
                    if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                    return CalculateSoundSpeedField(temperatureFields.Find(field => field.TimePeriod == month), salinityFields.Find(field => field.TimePeriod == month), bathymetry);
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = 1,
                });
            var batchBlock = new BatchBlock<SoundSpeedField>(temperaturePeriods.Count);
            transformBlock.LinkTo(batchBlock);
            transformBlock.Completion.ContinueWith(task => batchBlock.Complete());
            foreach (var month in temperaturePeriods) transformBlock.Post(month);
            transformBlock.Complete();
            await batchBlock.Completion;
            return batchBlock.Receive().ToList();
        }

        public static SoundSpeedField CalculateSoundSpeedField(SoundSpeedField temperatureField, SoundSpeedField salinityField, Bathymetry bathymetry = null)
        {
            var soundspeed = SoundSpeedField.Create(temperatureField, salinityField);
            if (bathymetry != null)
            {
                var deepestPoint = new EarthCoordinate<float>(bathymetry.Minimum, Math.Abs(bathymetry.Minimum.Data));
                soundspeed = soundspeed.Extend(temperatureField, salinityField, deepestPoint);
            }
            return soundspeed;
        }

        static void VerifyThatProfilePointsMatch(TimePeriodEnvironmentData<SoundSpeedProfile> profile1, TimePeriodEnvironmentData<SoundSpeedProfile> profile2)
        {
            foreach (var point1 in profile1.EnvironmentData.Where(point1 => !profile2.EnvironmentData.Any(point1.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at {0}, the other does not", point1));
            foreach (var point2 in profile2.EnvironmentData.Where(point2 => !profile1.EnvironmentData.Any(point2.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at {0}, the other does not", point2));
        }
#endif

        public static string FindSalinityFile(NAVOTimePeriod monthIndex)
        {
            var gdemRootDirectory = Globals.AppSettings.NAVOConfiguration.GDEMDirectory;
            var files = Directory.GetFiles(gdemRootDirectory, GDEMSalinityFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            files = Directory.GetFiles(gdemRootDirectory, NUWCSalinityFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            throw new FileNotFoundException(string.Format("Could not find requested salinity file, tried {0} and {1}", GDEMSalinityFileName(monthIndex), NUWCSalinityFileName(monthIndex)));
        }

        public static string FindTemperatureFile(NAVOTimePeriod monthIndex)
        {
            var gdemRootDirectory = Globals.AppSettings.NAVOConfiguration.GDEMDirectory;
            var files = Directory.GetFiles(gdemRootDirectory, GDEMTemperatureFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            files = Directory.GetFiles(gdemRootDirectory, NUWCTemperatureFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            throw new FileNotFoundException(string.Format("Could not find requested temperature file, tried {0} and {1}", GDEMTemperatureFileName(monthIndex), NUWCTemperatureFileName(monthIndex)));
        }

        static string GDEMTemperatureFileName(NAVOTimePeriod monthIndex) { return "t" + BaseGDEMFileName(monthIndex); }
        static string GDEMSalinityFileName(NAVOTimePeriod monthIndex) { return "s" + BaseGDEMFileName(monthIndex); }
        static string BaseGDEMFileName(NAVOTimePeriod monthIndex) { return "gdemv3s" + string.Format("{0:00}", (int)monthIndex) + ".nc"; }
        static string NUWCTemperatureFileName(NAVOTimePeriod monthIndex) { return ShortMonthNames[(int)monthIndex] + "_t.nc"; }
        static string NUWCSalinityFileName(NAVOTimePeriod monthIndex) { return ShortMonthNames[(int)monthIndex] + "_s.nc"; }
        static readonly string[] ShortMonthNames = new[] { "noneuary", "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec" };

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

            int lastLatIndex = -1, lastLonIndex = -1, lastDepthIndex = -1;
            var newFieldEnvironmentData = new List<SoundSpeedProfile>();
            Logger.Log("in ReadFile: 1");

            for (var lonIndex = 0; lonIndex < lonCount; lonIndex++)
            {
                lastLonIndex = lonIndex;
                var lon = lonMap[lonIndex].Value;
                var wrappedLon = lon;
                while (wrappedLon > 180) wrappedLon -= 360;
                while (wrappedLon < -180) wrappedLon += 360;
                Logger.Log("in ReadFile: 2");

                var lonSourceIndex = lonMap[lonIndex].Index;
                for (var latIndex = 0; latIndex < latCount; latIndex++)
                {
                    Logger.Log("in ReadFile: 3");
                    lastLatIndex = latIndex;
                    var lat = latMap[latIndex].Value;
                    var latSourceIndex = latMap[latIndex].Index;
                    var newProfile = new SoundSpeedProfile(new EarthCoordinate(lat, wrappedLon));
                    for (var depthIndex = 0; depthIndex < depths.Length; depthIndex++)
                    {
                        Logger.Log("in ReadFile: 4");
                        lastDepthIndex = depthIndex;
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
