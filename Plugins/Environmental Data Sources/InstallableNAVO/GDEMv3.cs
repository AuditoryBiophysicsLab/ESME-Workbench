using System;
using System.IO;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
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

        public SoundSpeed ExtractTemperature(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, NAVOConfiguration navoConfiguration = null, IProgress<float> progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0f);
            var temperatureField = GDEM.ReadFile(GDEM.FindTemperatureFile(timePeriod, DataLocation), "water_temp", timePeriod, geoRect);
            if (progress != null) lock (progress) progress.Report(50f);
            var temperature = new SoundSpeed();
            temperature.SoundSpeedFields.Add(temperatureField);
            if (progress != null) lock (progress) progress.Report(100f);
            return temperature;
        }

        public SoundSpeed ExtractSalinity(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, NAVOConfiguration navoConfiguration = null, IProgress<float> progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0f);
            var salinityField = GDEM.ReadFile(GDEM.FindSalinityFile(timePeriod, DataLocation), "salinity", timePeriod, geoRect);
            if (progress != null) lock (progress) progress.Report(50f);
            var salinity = new SoundSpeed();
            salinity.SoundSpeedFields.Add(salinityField);
            if (progress != null) lock (progress) progress.Report(100f);
            return salinity;
        }

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, NAVOConfiguration navoConfiguration = null, IProgress<float> progress = null)
        {
            throw new NotImplementedException(string.Format("{0} extraction routine requires either deepest point OR bathymetry argument", PluginName));
        }

        public SoundSpeed Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, EarthCoordinate<float> deepestPoint, NAVOConfiguration navoConfiguration = null, IProgress<float> progress = null)
        {
            if (progress != null) lock (progress) progress.Report(0);
            var curProgressStep = 0f;
            var intermediateProgress = new Progress<float>(prog =>
            {
                if (progress != null) lock (progress) progress.Report(curProgressStep + (prog / 3));
            });
            var temperatureData = ExtractTemperature(geoRect, resolution, timePeriod, navoConfiguration, intermediateProgress);
            curProgressStep = 33f;
            var salinityData = ExtractSalinity(geoRect, resolution, timePeriod, navoConfiguration, intermediateProgress);
            curProgressStep = 66f;
            var result = SoundSpeed.Create(temperatureData, salinityData, deepestPoint, intermediateProgress);
            if (progress != null) lock (progress) progress.Report(100f);
            return result;
        }

        public SoundSpeed Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, Bathymetry bathymetry, NAVOConfiguration navoConfiguration = null, IProgress<float> progress = null) 
        {
            return Extract(geoRect, resolution, timePeriod, bathymetry.DeepestPoint, navoConfiguration, progress);
        }
    }
}
