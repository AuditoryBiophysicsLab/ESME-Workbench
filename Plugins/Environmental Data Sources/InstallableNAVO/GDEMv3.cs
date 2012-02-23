using System;
using System.IO;
using System.Linq;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Plugins;
using HRC.Navigation;
using InstallableNAVO.Controls;
using Microsoft.Win32;

namespace InstallableNAVO
{
    public sealed class GDEM3 : EnvironmentalDataSourcePluginBase<SoundSpeed>
    {
        public GDEM3()
        {
            PluginName = "GDEM-V 3.0";
            PluginDescription = "Generalized Digital Environment Model, Variable Resolution, version 3.0, from US Navy/NAVOCEANO";
            DataLocationHelp = "A directory containing the GDEM-V 3.0 NetCDF files with temperature and salinity data (tgdemv3s01.nc for example)";
            IsTimeVariantData = true;
            AvailableTimePeriods = NAVOConfiguration.AllMonths.ToArray();
            PluginType = PluginType.EnvironmentalDataSource;
            AvailableResolutions = new float[] { 15 };
            Subtype = "Sound Speed";
            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\GDEM-V 3.0");
            if (regKey != null) DataLocation = (string)regKey.GetValue("");
            ConfigurationControl = new GDEM3Configuration {DataContext = this};

            IsSelectable = DataLocation != null;
            IsConfigured = DataLocation != null &&
                           Directory.Exists(DataLocation) &&
                           Databases.GDEM.IsDirectoryValid(DataLocation);
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

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            var result = new SoundSpeed();
            result.Add(Databases.GDEM.ReadFile(DataLocation, timePeriod, geoRect));
            return result;
        }
    }
}
