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
    public sealed class DBDB54 : EnvironmentalDataSourcePluginBase<Bathymetry>
    {
        const string RequiredDBDBFilename = "dbdbv5_level0c_0.h5";

        public DBDB54() 
        {
            PluginName = "DBDB-V 5.4";
            PluginDescription = "Digital Bathymetric Data Base - Variable Resolution v5.4, from US Navy/NAVOCEANO";
            DataLocationHelp = "A file called dbdbv5_level0c_0.h5";
            //ConfigurationControl = new GDEM3Configuration { DataContext = this };
            PluginType = PluginType.EnvironmentalDataSource;
            Resolutions = new[] { 2, 1, 0.5f, 0.1f, 0.05f };
            IsTimeVariantData = false;
            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\DBDB-V 5.4");
            if (regKey != null) DataLocation = (string)regKey.GetValue("");

            IsSelectable = DataLocation != null;
            IsConfigured = DataLocation != null &&
                           Directory.Exists(DataLocation) &&
                           File.Exists(Path.Combine(DataLocation, RequiredDBDBFilename));
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

        public override Bathymetry Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod, IProgress<float> progress = null) 
        {
            return DBDB.Extract(resolution, geoRect, progress);
        }
    }
}
