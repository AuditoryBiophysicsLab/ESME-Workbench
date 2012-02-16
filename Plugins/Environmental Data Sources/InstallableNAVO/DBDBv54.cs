using System;
using System.Collections.Generic;
using System.IO;
using ESME;
using ESME.Environment;
using ESME.Environment.NAVO;
using ESME.Plugins;
using HRC.Navigation;
using HRC.Validation;
using NAVO.Controls;

namespace NAVO
{
    public sealed class DBDB54 : EnvironmentalDataSourcePluginBase<Bathymetry>
    {
        const string RequiredDBDBFilename = "dbdbv5_level0c_0.h5";

        public DBDB54() 
        {
            PluginName = "DBDB v5.4";
            PluginDescription = "Digital Bathymetric Data Base - Variable Resolution v5.4, from US Navy/NAVOCEANO";
            DataLocationHelp = "A file called dbdbv5_level0c_0.h5";
            ConfigurationControl = new GDEM3Configuration { DataContext = this };
            PluginType = PluginType.EnvironmentalDataSource;
            Resolutions = new float[] { 4 };

            ValidationRules.AddRange(new List<ValidationRule>
            {
                new ValidationRule
                {
                    PropertyName = "DataLocation",
                    Description = "File must exist and be named dbdbv5_level0c_0.h5",
                    RuleDelegate = (o, r) => ((GDEM3)o).DataLocation != null && Directory.Exists(((GDEM3)o).DataLocation) && File.Exists(Path.Combine(DataLocation, RequiredDBDBFilename)),
                },
            });
            
        }

        public override Bathymetry Extract(GeoRect geoRect, float resolution, NAVOTimePeriod timePeriod, IProgress<float> progress = null)
        {
            throw new NotImplementedException();
        }
    }
}
