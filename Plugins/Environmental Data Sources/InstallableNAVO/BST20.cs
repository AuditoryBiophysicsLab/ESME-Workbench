using System;
using System.IO;
using ESME;
using ESME.Environment;
using ESME.Plugins;
using HRC.Navigation;
using Microsoft.Win32;

namespace InstallableNAVO
{
    public sealed class BST20 : EnvironmentalDataSourcePluginBase<Sediment>
    {
        const string RequiredBSTFilename = "hfevav2.h5";

        public BST20()
        {
            PluginName = "BST 2.0";
            PluginDescription = "Bottom Sediments Type Database Version 2.0 Repacked , from US Navy/NAVOCEANO";
            DataLocationHelp = "A file called hfevav2.h5";
            //ConfigurationControl = new GDEM3Configuration { DataContext = this };
            PluginType = PluginType.EnvironmentalDataSource;
            AvailableResolutions = new[] { 5f };
            IsTimeVariantData = false;
            AvailableTimePeriods = new[] { TimePeriod.Invalid };
            Subtype = "Sediment";

            var regKey = Registry.LocalMachine.OpenSubKey(@"Software\Boston University\ESME Workbench\Data Sources\BST 2.0");
            if (regKey != null) DataLocation = (string)regKey.GetValue("");

            IsSelectable = DataLocation != null;
            IsConfigured = DataLocation != null &&
                           Directory.Exists(DataLocation) &&
                           File.Exists(Path.Combine(DataLocation, RequiredBSTFilename));
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

        public override Sediment Extract(GeoRect geoRect, float resolution, TimePeriod timePeriod = TimePeriod.Invalid, IProgress<float> progress = null)
        {
            CheckResolutionAndTimePeriod(resolution, timePeriod);
            return Databases.BST.Extract(DataLocation, geoRect, resolution, progress);
        }
    }
}