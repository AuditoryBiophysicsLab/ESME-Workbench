using System;
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

        public override SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, SeasonConfiguration seasonConfiguration = null, IProgress<float> progress = null)
        {
            throw new NotImplementedException(string.Format("{0} extraction routine requires either deepest point OR bathymetry argument", PluginName));
        }

        public SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, Geo<float> deepestPoint, SeasonConfiguration seasonConfiguration, IProgress<float> progress = null)
        {
            var months = seasonConfiguration.MonthsInTimePeriod(timePeriod).ToList();
            var monthlySoundSpeeds = (from month in months
                                      select new
                                      {
                                          Month = month,
                                          SoundSpeedTask = new Task<SoundSpeedField>(() => GDEM.ReadFile(DataLocation, month, geoRect))
                                      }).ToDictionary(item => item.Month);
            var averageSoundSpeeds = new SoundSpeed();
            foreach (var month in months) averageSoundSpeeds.Add(monthlySoundSpeeds[month].SoundSpeedTask.Result);
            //return SoundSpeed<GDEMSoundSpeedSample>.Average(averageSoundSpeeds, new List<TimePeriod> { timePeriod });
            return null;
        }

        public SoundSpeed Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, Bathymetry bathymetry, SeasonConfiguration seasonConfiguration, IProgress<float> progress = null) 
        {
            return Extract(geoRect, resolution, timePeriod, bathymetry.DeepestPoint, seasonConfiguration, progress);
        }
    }
}
