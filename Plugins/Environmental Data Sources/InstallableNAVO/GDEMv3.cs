using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using ESME;
using ESME.Environment;
using ESME.Plugins;
using HRC.Navigation;
using Microsoft.Win32;
using GDEM = InstallableNAVO.Databases.GDEM;

namespace InstallableNAVO
{
    public sealed class GDEM3 : EnvironmentalDataSourcePluginBase<SoundSpeed>, IGDEM3DataSource<SoundSpeed>
    {
        public GDEM3()
        {
            PluginName = "GDEM-V 3.0";
            PluginDescription = "Generalized Digital Environment Model, Variable Resolution, version 3.0, from US Navy/NAVOCEANO";
            DataLocationHelp = "A directory containing the GDEM-V 3.0 NetCDF files with temperature and salinity data (tgdemv3s01.nc for example)";
            PluginType = PluginType.EnvironmentalDataSource;
            Resolutions = new float[] { 15 };
            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\GDEM-V 3.0");
            if (regKey != null) DataLocation = (string)regKey.GetValue("");

            IsSelectable = DataLocation != null;
            IsConfigured = DataLocation != null &&
                           Directory.Exists(DataLocation) &&
                           GDEM.IsDirectoryValid(DataLocation);
#if false
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "DataLocation",
                    Description = "Directory must exist and contain 24 appropriate GDEM NetCDF files (names like [t|s]gdemv3s[01-12].nc)",
                    RuleDelegate = (o, r) => ((GDEM3)o).DataLocation != null && GDEM.IsDirectoryValid(((GDEM3)o).DataLocation),
                },
            });
#endif
        }

        public SoundSpeed ExtractTemperature(GeoRect geoRect, float resolution, TimePeriod timePeriod, SeasonConfiguration seasonConfiguration, IProgress<float> progress = null)
        {
            return ExtractVariable(GDEM.FindTemperatureFile, "water_temp", geoRect, timePeriod, seasonConfiguration);
        }

        public SoundSpeed ExtractSalinity(GeoRect geoRect, float resolution, TimePeriod timePeriod, SeasonConfiguration seasonConfiguration, IProgress<float> progress = null)
        {
            return ExtractVariable(GDEM.FindTemperatureFile, "salinity", geoRect, timePeriod, seasonConfiguration);
        }

        SoundSpeed ExtractVariable(Func<TimePeriod, string, string> fileNameFunc, string variableName, GeoRect geoRect, TimePeriod timePeriod, SeasonConfiguration seasonConfiguration)
        {
            var months = seasonConfiguration.MonthsInTimePeriod(timePeriod).ToList();
            var sources = (from month in months
                           select new
                           {
                               Month = month,
                               DataTask = new Task<SoundSpeedField>(() => GDEM.ReadFile(fileNameFunc(month, DataLocation), variableName, month, geoRect)),
                           }).ToDictionary(item => item.Month);
            var sourceTasks = new List<Task>();
            foreach (var month in months)
            {
                sources[month].DataTask.Start();
                sourceTasks.Add(sources[month].DataTask);
            }
            var continuation = TaskEx.WhenAll(sourceTasks).ContinueWith(task =>
            {
                var result = new SoundSpeed();
                foreach (var month in months) result.Add(sources[month].DataTask.Result);
                return result;
            });
            return continuation.Result;
        }

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, SeasonConfiguration seasonConfiguration = null, IProgress<float> progress = null)
        {
            throw new NotImplementedException(string.Format("{0} extraction routine requires either deepest point OR bathymetry argument", PluginName));
        }

        public SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, EarthCoordinate<float> deepestPoint, SeasonConfiguration seasonConfiguration, IProgress<float> progress = null)
        {
            var temperatureTask = new Task<SoundSpeed>(() => ExtractTemperature(geoRect, resolution, timePeriod, seasonConfiguration, progress));
            var salinityTask = new Task<SoundSpeed>(() => ExtractSalinity(geoRect, resolution, timePeriod, seasonConfiguration, progress));
            temperatureTask.Start();
            salinityTask.Start();
            var continuation = TaskEx.WhenAll(temperatureTask, salinityTask).ContinueWith(task =>
            {
                var months = seasonConfiguration.MonthsInTimePeriod(timePeriod).ToList();
                var soundSpeedFields = (from month in months
                                        select new
                                        {
                                            Month = month,
                                            SoundSpeedField = SoundSpeedField.Create(temperatureTask.Result[month],
                                                                                     salinityTask.Result[month],
                                                                                     deepestPoint,
                                                                                     geoRect),
                                        }).ToDictionary(item => item.Month);
                temperatureTask.Dispose();
                salinityTask.Dispose();
                var monthlySoundSpeeds = new SoundSpeed();
                foreach (var month in months) monthlySoundSpeeds.Add(soundSpeedFields[month].SoundSpeedField);
                var result = SoundSpeed.Average(monthlySoundSpeeds, new List<TimePeriod> { timePeriod });
                return result;
            });
            return continuation.Result;        
        }

        public SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, Bathymetry bathymetry, SeasonConfiguration seasonConfiguration, IProgress<float> progress = null) 
        {
            return Extract(geoRect, resolution, timePeriod, bathymetry.DeepestPoint, seasonConfiguration, progress);
        }
    }
}
