﻿using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using System.Threading.Tasks.Dataflow;
using HRC.Navigation;
using HRC.NetCDF;

namespace ESME.Environment.NAVO
{
    public static class GDEM
    {
        public static async Task<SoundSpeed> ReadTemperatureAsync(ICollection<NAVOTimePeriod> months, float north, float south, float east, float west, IProgress<float> progress = null)
        {
            var progressStep = 100f / (months.Count + 1);
            var totalProgress = 0f;
            var transformBlock = new TransformBlock<Tuple<NAVOTimePeriod, float, float, float, float>, SoundSpeedField>(data =>
                {
                    var temp = ReadTemperature(data.Item1, data.Item2, data.Item3, data.Item4, data.Item5);
                    if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                    return temp;
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = DataflowBlockOptions.Unbounded
                });
            var batchBlock = new BatchBlock<SoundSpeedField>(months.Count);
            transformBlock.LinkTo(batchBlock);
            foreach (var month in months) transformBlock.Post(new Tuple<NAVOTimePeriod, float, float, float, float>(month, north, south, east, west));
            transformBlock.Complete();
            var result = new SoundSpeed {SoundSpeedFields = batchBlock.Receive().ToList()};
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            return result;
        }

        public static SoundSpeed ReadTemperature(List<NAVOTimePeriod> months, float north, float south, float east, float west)
        {
            var result = new SoundSpeed();
            foreach (var month in months) result.SoundSpeedFields.Add(ReadTemperature(month, north, south, east, west));
            return result;
        }

        public static Task<SoundSpeedField> ReadTemperatureAsync(NAVOTimePeriod month, float north, float south, float east, float west)
        {
            return TaskEx.Run(() => ReadTemperature(month, north, south, east, west));
        }

        public static SoundSpeedField ReadTemperature(NAVOTimePeriod month, float north, float south, float east, float west)
        {
            return ReadFile(FindTemperatureFile(month), "water_temp", month, north, south, east, west);
        }

        public async static Task<SoundSpeed> ReadSalinityAsync(ICollection<NAVOTimePeriod> months, float north, float south, float east, float west, IProgress<float> progress = null)
        {
            var progressStep = 100f / (months.Count + 1);
            var totalProgress = 0f;
            var transformBlock = new TransformBlock<Tuple<NAVOTimePeriod, float, float, float, float>, SoundSpeedField>(data => 
                {
                    var salinity = ReadSalinity(data.Item1, data.Item2, data.Item3, data.Item4, data.Item5);
                    if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                    return salinity;
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = DataflowBlockOptions.Unbounded
                });
            var batchBlock = new BatchBlock<SoundSpeedField>(months.Count);
            transformBlock.LinkTo(batchBlock);
            foreach (var month in months) transformBlock.Post(new Tuple<NAVOTimePeriod, float, float, float, float>(month, north, south, east, west));
            transformBlock.Complete();
            var result = new SoundSpeed { SoundSpeedFields = batchBlock.Receive().ToList() };
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            return result;
        }

        public static SoundSpeed ReadSalinity(List<NAVOTimePeriod> months, float north, float south, float east, float west)
        {
            var result = new SoundSpeed();
            foreach (var month in months) result.SoundSpeedFields.Add(ReadSalinity(month, north, south, east, west));
            return result;
        }

        public static Task<SoundSpeedField> ReadSalinityAsync(NAVOTimePeriod month, float north, float south, float east, float west)
        {
            return TaskEx.Run(() => ReadSalinity(month, north, south, east, west));
        }

        public static SoundSpeedField ReadSalinity(NAVOTimePeriod month, float north, float south, float east, float west)
        {
            return ReadFile(FindSalinityFile(month), "salinity", month, north, south, east, west);
        }

        public async static Task<SoundSpeed> ReadSoundSpeedAsync(ICollection<NAVOTimePeriod> months, float north, float south, float east, float west, EarthCoordinate<float> deepestPoint = null, IProgress<float> progress = null)
        {
            var progressStep = 100f / (months.Count + 1);
            var totalProgress = 0f;
            var transformBlock = new TransformBlock<Tuple<NAVOTimePeriod, float, float, float, float, EarthCoordinate<float>>, SoundSpeedField>(data =>
                {
                    var soundSpeed = ReadSoundSpeed(data.Item1, data.Item2, data.Item3, data.Item4, data.Item5, data.Item6);
                    if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                    return soundSpeed;
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = DataflowBlockOptions.Unbounded
                });
            var batchBlock = new BatchBlock<SoundSpeedField>(months.Count);
            transformBlock.LinkTo(batchBlock);
            foreach (var month in months) transformBlock.Post(new Tuple<NAVOTimePeriod, float, float, float, float, EarthCoordinate<float>>(month, north, south, east, west, deepestPoint));
            transformBlock.Complete();
            var result = new SoundSpeed { SoundSpeedFields = batchBlock.Receive().ToList() };
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            return result;
        }

        public static SoundSpeed ReadSoundSpeed(List<NAVOTimePeriod> months, float north, float south, float east, float west, EarthCoordinate<float> deepestPoint = null)
        {
            var result = new SoundSpeed();
            foreach (var month in months) result.SoundSpeedFields.Add(ReadSoundSpeed(month, north, south, east, west, deepestPoint));
            return result;
        }

        public static SoundSpeedField ReadSoundSpeed(NAVOTimePeriod month, float north, float south, float east, float west, EarthCoordinate<float> deepestPoint = null)
        {
            var temperature = ReadTemperature(month, north, south, east, west);
            var salinity = ReadSalinity(month, north, south, east, west);
            var soundspeed = SoundSpeedField.Create(temperature, salinity);
            if (deepestPoint != null) soundspeed = soundspeed.Extend(temperature, salinity, deepestPoint);
            return soundspeed;
        }

        public async static Task<SoundSpeed> CalculateSoundSpeedAsync(SoundSpeed temperature, SoundSpeed salinity, EarthCoordinate<float> deepestPoint = null, IProgress<float> progress = null)
        {
            var progressStep = 100f / (temperature.SoundSpeedFields.Count + 1);
            var totalProgress = 0f;
            var transformBlock = new TransformBlock<Tuple<SoundSpeedField, SoundSpeedField, EarthCoordinate<float>>, SoundSpeedField>(data =>
                {
                    var soundSpeed = CalculateSoundSpeed(data.Item1, data.Item2, data.Item3);
                    if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
                    return soundSpeed;
                },
                new ExecutionDataflowBlockOptions
                {
                    TaskScheduler = TaskScheduler.Default,
                    MaxDegreeOfParallelism = DataflowBlockOptions.Unbounded
                });
            var batchBlock = new BatchBlock<SoundSpeedField>(temperature.SoundSpeedFields.Count);
            transformBlock.LinkTo(batchBlock);
            foreach (var field in temperature.SoundSpeedFields) transformBlock.Post(new Tuple<SoundSpeedField, SoundSpeedField, EarthCoordinate<float>>(field, salinity[field.TimePeriod], deepestPoint));
            transformBlock.Complete();
            var result = new SoundSpeed { SoundSpeedFields = batchBlock.Receive().ToList() };
            if (progress != null) lock (progress) progress.Report(totalProgress += progressStep);
            return result;
        }

        public static SoundSpeed CalculateSoundSpeed(SoundSpeed temperature, SoundSpeed salinity, EarthCoordinate<float> deepestPoint = null)
        {
            var result = new SoundSpeed();
            foreach (var field in temperature.SoundSpeedFields)
                result.SoundSpeedFields.Add(CalculateSoundSpeed(field, salinity[field.TimePeriod], deepestPoint));
            return result;
        }

        public static SoundSpeedField CalculateSoundSpeed(SoundSpeedField temperatureField, SoundSpeedField salinityField, EarthCoordinate<float> deepestPoint = null)
        {
            if (temperatureField.TimePeriod != salinityField.TimePeriod) throw new DataException("time period mismatch");
            VerifyThatProfilePointsMatch(temperatureField, salinityField);
            var environmentData = temperatureField.EnvironmentData.Select(temperatureProfile => ChenMilleroLi.SoundSpeed(temperatureProfile, salinityField.EnvironmentData[temperatureProfile]));
            var soundSpeedField = new SoundSpeedField { TimePeriod = temperatureField.TimePeriod };
            soundSpeedField.EnvironmentData.AddRange(environmentData);

            if (deepestPoint != null)
            {
                VerifyThatProfilePointsMatch(temperatureField, salinityField);
                VerifyThatProfilePointsMatch(temperatureField, soundSpeedField);

                soundSpeedField.DeepestPoint = deepestPoint;
                var deepestSSP = soundSpeedField.DeepestSSP;
                if (deepestPoint.Data > soundSpeedField.DeepestSSP.Data.MaxDepth)
                    deepestSSP.Extend(deepestPoint.Data, temperatureField.EnvironmentData[deepestSSP], salinityField.EnvironmentData[deepestSSP]);
                foreach (var profile in soundSpeedField.EnvironmentData.Where(profile => profile != deepestSSP))
                    profile.Extend(deepestSSP);
            }
            return soundSpeedField;
        }

        static void VerifyThatProfilePointsMatch(TimePeriodEnvironmentData<SoundSpeedProfile> profile1, TimePeriodEnvironmentData<SoundSpeedProfile> profile2)
        {
            foreach (var point1 in profile1.EnvironmentData.Where(point1 => !profile2.EnvironmentData.Any(point1.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at {0}, the other does not", point1));
            foreach (var point2 in profile2.EnvironmentData.Where(point2 => !profile1.EnvironmentData.Any(point2.Equals))) throw new DataException(string.Format("Profiles do not contain the same data points.  One has data at {0}, the other does not", point2));
        }

        static string FindSalinityFile(NAVOTimePeriod monthIndex)
        {
            var gdemRootDirectory = Globals.AppSettings.NAVOConfiguration.GDEMDirectory;
            var files = Directory.GetFiles(gdemRootDirectory, GDEMSalinityFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            files = Directory.GetFiles(gdemRootDirectory, NUWCSalinityFileName(monthIndex), SearchOption.AllDirectories);
            if (files.Length > 0) return files[0];
            throw new FileNotFoundException(string.Format("Could not find requested salinity file, tried {0} and {1}", GDEMSalinityFileName(monthIndex), NUWCSalinityFileName(monthIndex)));
        }

        static string FindTemperatureFile(NAVOTimePeriod monthIndex)
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

        static SoundSpeedField ReadFile(string fileName, string dataVarName, NAVOTimePeriod month, float north, float south, float east, float west)
        {
            var myFile = new NetCDF(fileName);
            var lats = ((NcVarDouble)myFile.Variables.Find(var => var.Name == "lat")).ToArray();
            var lons = ((NcVarDouble)myFile.Variables.Find(var => var.Name == "lon")).ToArray();
            var depths = ((NcVarDouble)myFile.Variables.Find(var => var.Name == "depth")).ToArray();
            var data = (NcVarShort)myFile.Variables.Find(var => var.Name == dataVarName);
            var missingValue = ((NcAttShort)data.Attributes.Find(att => att.Name == "missing_value"))[0];
            var scaleFactor = ((NcAttFloat)data.Attributes.Find(att => att.Name == "scale_factor"))[0];
            var addOffset = ((NcAttFloat)data.Attributes.Find(att => att.Name == "add_offset"))[0];

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

            for (var lonIndex = 0; lonIndex < lonCount; lonIndex++)
            {
                var lon = lonMap[lonIndex].Value;
                var wrappedLon = lon;
                while (wrappedLon > 180) wrappedLon -= 360;
                while (wrappedLon < -180) wrappedLon += 360;

                var lonSourceIndex = lonMap[lonIndex].Index;
                for (var latIndex = 0; latIndex < latCount; latIndex++)
                {
                    var lat = latMap[latIndex].Value;
                    var latSourceIndex = latMap[latIndex].Index;
                    var newProfile = new SoundSpeedProfile(new EarthCoordinate(lat, wrappedLon));
                    for (var depthIndex = 0; depthIndex < depths.Length; depthIndex++)
                    {
                        var curValue = data[(uint)depthIndex, (uint)latSourceIndex, (uint)lonSourceIndex];
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