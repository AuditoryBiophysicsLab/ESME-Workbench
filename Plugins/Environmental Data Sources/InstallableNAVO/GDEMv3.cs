using System;
using System.IO;
using System.Linq;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.NEMO;
using ESME.Plugins;
using HRC.Navigation;
using Microsoft.Win32;
using GDEM = InstallableNAVO.Databases.GDEM;

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

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, IProgress<float> progress = null)
        {
            if (!AvailableTimePeriods.Contains(timePeriod)) throw new ParameterOutOfRangeException(string.Format("Specified timePeriod is not available in the {0} data set", PluginName));
            var result = new SoundSpeed();
            result.Add(GDEM.ReadFile(DataLocation, timePeriod, geoRect));
            return result;
        }
    }
}
