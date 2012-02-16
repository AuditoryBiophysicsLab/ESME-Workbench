using System;
using System.IO;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Plugins;
using HRC.Navigation;
using Microsoft.Win32;

namespace InstallableNAVO
{
    public sealed class SMGC20Minimal : EnvironmentalDataSourcePluginBase<Wind>
    {
        const string RequiredSMGCFilename = "smgc.wind";

        public SMGC20Minimal()
        {
            PluginName = "SMGC 2.0 Minimal";
            PluginDescription = "Surface Marine Gridded Climatology Database v2.0, from US Navy/NAVOCEANO";
            DataLocationHelp = "A file called smgc.wind";
            //ConfigurationControl = new GDEM3Configuration { DataContext = this };
            PluginType = PluginType.EnvironmentalDataSource;
            Resolutions = new[] { 60f };
            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\SMGC 2.0 Minimal");
            if (regKey != null) DataLocation = (string)regKey.GetValue("");

            IsSelectable = DataLocation != null;
            IsConfigured = DataLocation != null &&
                           Directory.Exists(DataLocation) &&
                           File.Exists(Path.Combine(DataLocation, RequiredSMGCFilename));
#if false
            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "DataLocation",
                    Description = "File must exist and be named dbdbv5_level0c_0.h5",
                    RuleDelegate = (o, r) => ((DBDB54)o).DataLocation != null && Directory.Exists(((DBDB54)o).DataLocation) && File.Exists(Path.Combine(DataLocation, RequiredDBDBFilename)),
                },
            });
#endif
        }

        public override Wind Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, NAVOConfiguration navoConfiguration = null, IProgress<float> progress = null)
        {
            var globalDataset = Wind.Load(Path.Combine(DataLocation, RequiredSMGCFilename));
            var result = new Wind();
            return result;
        }
    }
}
